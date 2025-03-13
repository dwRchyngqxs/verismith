-- Module      : Verismith.Verilog2005.Mutation
-- Description : AST mutation.
-- Copyright   : (c) 2024 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE OverloadedLists #-}

module Verismith.Verilog2005.Mutation
  ( runMutation,
  )
where

import Numeric.Natural
import Data.Bits
import Data.Typeable
import Data.Maybe
import Data.Functor.Identity
import Data.ByteString.Internal (packChars)
import Data.List.NonEmpty (NonEmpty)
import Data.Bifunctor (second)
import Data.Bitraversable (bitraverse)
import GHC.IsList
import System.Random.MWC.Probability
import Control.Monad
import Control.Monad.Reader
import Verismith.Utils (mkpair)
import Verismith.Config (MutationOpts (..), Config (..))
import Verismith.Verilog2005.Randomness
import Verismith.Verilog2005.AST
import qualified Verismith.Verilog2005.EvalElabAST as EE
import Verismith.Verilog2005.Utils

data MutationStore = MutationStore
  { _msMTM :: !(forall t. Eq t => MutationVector (MinTypMax t)),
    _msPrim :: !(forall i r a. MutationVector (Prim i r a)),
    _msExpr :: !(forall i r a. MutationVector (Expr i r a)),
    _msAttr :: !(MutationVector Attributes),
    _msRangeExpr :: !(forall t. MutationVector (RangeExpr t CExpr)),
    _msLValue :: !(forall t. MutationVector (LValue t CExpr)),
    _msAssign :: !(forall t. MutationVector (Assign t CExpr NExpr)),
    _msEventPrim :: !(MutationVector (EventPrim NExpr)),
    _msDelay1 :: !(MutationVector (Delay1 NExpr)),
    _msDelay2 :: !(MutationVector (Delay2 NExpr)),
    _msDelay3 :: !(MutationVector (Delay3 NExpr)),
    _msLoopStmt :: !(MutationVector (LoopStatement NExpr CExpr)),
    _msCasePat :: !(forall t. MutationVector (NonEmpty t)),
    _msStmt :: !(MutationVector (Statement NExpr CExpr)),
    _msGenCond :: !(MutationVector (ModGenCondItem NExpr CExpr)),
    _msGenBlk :: !(MutationVector (GenerateBlock NExpr CExpr)),
    _msModGenItem :: !(MutationVector (ModGenBlockedItem NExpr CExpr)),
    _msModItem :: !(MutationVector (ModuleItem NExpr CExpr MPExpr)),
    _msPDV :: !(MutationVector (PathDelayValue CExpr)),
    _msSpecItem :: !(MutationVector (SpecifyBlockedItem NExpr CExpr MPExpr)),
    _msModule :: !(MutationVector (ModuleBlock NExpr CExpr MPExpr)),
    _msPrimTable :: !(MutationVector PrimTable),
    _msPrimitive :: !(MutationVector (PrimitiveBlock CExpr)),
    _msV2005 :: !(MutationVector Verilog2005),
    _msList :: !(forall t. IsList t => MutationVector t)
  }

type Mutator = GenM MutationStore
type PureMutation t = t -> Maybe t
type PrimMutation t = t -> Maybe (Mutator t)
type MutationVector t = [(Double, PrimMutation t)]
type Mutation t = t -> Mutator t

-- Mutation list:
-- Identifier: Need to build the name hierarchy, only doable at module/Primitive/Generate/Config/function/whatever level
-- Prim-> conversion of constant to a specific representation, concat one, multconcat from concat
-- Function changes at global level
-- Expression splitting at MGI level
-- Concat reordering then fixing the references at global level
-- Expr-> ternary branch swapping, "-"-"+ -", "*1"-"", "/1"-"", "+0"-"", constant folding,
--   "*x/x"-"", plus assoc, plus commut, times commut, times assoc, "~ +1"-"-", "- -1"-"~", "un+"-"",
--   "!"-"&~", "!"-"~|", "|"-"!=0", "|~"-"~&", "~ &"-"~&", "~ |"-"~|", "! |"-"~|", "! &"-"~&",
--   "~ ^"-"~^", "! ^"-"~^", "~ ~"-""
-- Some may be incorrect because of signedness
-- type aware rewrites? neq is |xor, "- -"-"", all rewrites in my ESA
-- GenRangeExpr-> make single, make pair, make baseoff+ and baseoff
-- SignRange: Change range and range references, change signedness and correct at callsite
-- Assignment: At Global Level, rename; if never referenced elswhere, Merge Expr; At local level, Split Expr
-- Parameter: At global level, rename, shuffle, offset the value at assign and deoffset at use
-- ParamOver (DefParam): At MGI level, change the value inside the module instantiation and put the real value in a defparam, resolve the defparam
-- (Param/Port)Assign: At global level or with enough info, change a named for a positional and the other way around
-- EventPrim-> change edge and expression accordingly
-- EventControl: at statement level, Deps <-> Expr
-- Statement: Check ESA, if/case-loop, to for, to while, to repeat
-- BlockDecl: At Block level, rename
-- ModGenCondItem-> If-Case comversion, If transforms, Case-transforms, check ESA
-- ModGenItem-> See ESA, gate conversion, always-initial forever, cond-loop
-- PathDelayValue-> refer to conversion table

-- TODO LATER: options
buildStore :: MutationOpts -> MutationStore
buildStore _ = MutationStore {
    _msMTM = [(1.0, mkPrim mtmTo1), (1.0, mkPrim mtmTo3)],
    _msPrim = [], -- [(1.0, ), ]
    _msExpr = [], -- [(1.0, ), ]
    _msAttr = [], -- [(1.0, ), ]
    _msRangeExpr = [], -- [(1.0, ), ]
    _msLValue = [], -- [(1.0, ), ]
    _msAssign = [], -- [(1.0, ), ]
    _msEventPrim = [], -- [(1.0, ), ]
    _msDelay1 = [(1.0, mkPrim delay1ToBase), (1.0, mkPrim delay1To1)],
    _msDelay2 = [(1.0, mkPrim delay2ToBase), (1.0, mkPrim delay2To1), (1.0, mkPrim delay2To2)],
    _msDelay3 =
      [ (1.0, mkPrim delay3ToBase),
        (1.0, mkPrim delay3To1),
        (1.0, mkPrim delay3To2),
        (1.0, mkPrim delay3To3)
      ],
    _msLoopStmt = [(1.0, mkPrim loopForever), (1.0, loopRepeat), (1.0, loopWhile)],
    _msCasePat = [], -- [(1.0, ), ]
    _msStmt = [], -- [(1.0, ), ]
    _msGenCond = [], -- [(1.0, ), ]
    _msGenBlk = [], -- [(1.0, ), ]
    _msModGenItem = [], -- [(1.0, ), ]
    _msModItem = [], -- [(1.0, ), ]
    _msPDV = [], -- [(1.0, ), ]
    _msSpecItem = [], -- [(1.0, ), ]
    _msModule = [], -- [(1.0, ), ]
    _msPrimTable = [], -- [(1.0, ), ]
    _msPrimitive = [], -- [(1.0, ), ]
    _msV2005 = [], -- [(1.0, ), ]
    _msList = [(1.0, shuffleList)]
  }
  where
    mkPrim f = fmap pure . f

mutate :: MutationVector t -> Mutation t
mutate v x = join $ sampleWeighted $ mapMaybe (traverse ($ x)) v

mutateWith :: (MutationStore -> MutationVector t) -> Mutation t
mutateWith p x = asks (p . fst) >>= flip mutate x

mutateList :: IsList t => Mutation t
mutateList l = fromList <$> mutateWith _msList (toList l)

mutateMTM :: Eq t => Mutation t -> Mutation (MinTypMax t)
mutateMTM f = mutateWith _msMTM >=> \x -> case x of
  MTMSingle e -> MTMSingle <$> f e
  MTMFull em et eM -> MTMFull <$> f em <*> f et <*> f eM

mutateCMTM :: Mutation (MinTypMax CExpr)
mutateCMTM = mutateMTM mutateCExpr

mutateNMTM :: Mutation (MinTypMax NExpr)
mutateNMTM = mutateMTM mutateNExpr

-- TODO MAYBE: elaborate expr then mutate
mutatePrim :: (Eq i, Eq r, Eq a) => Mutation i -> Mutation r -> Mutation a -> Mutation (Prim i r a)
mutatePrim fi fr fa = mutateWith _msPrim >=> \x -> case x of
  PrimIdent i r -> PrimIdent <$> fi i <*> fr r
  PrimConcat c -> PrimConcat <$> mapM mExpr c
  PrimMultConcat m c ->
    PrimMultConcat <$> mutateExpr pure (traverse mutateCRE) fa m <*> mapM mExpr c
  PrimFun i a args -> PrimFun <$> fi i <*> fa a <*> mapM mExpr args
  PrimSysFun i args -> PrimSysFun i <$> mapM mExpr args
  PrimMinTypMax m -> PrimMinTypMax <$> mutateMTM mExpr m
  _ -> pure x
  where mExpr = mutateExpr fi fr fa

mutateHI :: Mutation (HierIdent CExpr)
mutateHI (HierIdent p i) = flip HierIdent i <$> mapM (traverse $ traverse mutateCExpr) p

mutateDR :: Mutation e -> Mutation (DimRange e CExpr)
mutateDR f (DimRange d r) = DimRange <$> mapM f d <*> mutateRE f r

mutateNDR :: Mutation (DimRange NExpr CExpr)
mutateNDR = mutateDR mutateNExpr

mutateCDR :: Mutation (DimRange CExpr CExpr)
mutateCDR = mutateDR mutateCExpr

-- TODO MAYBE: elaborate expr then mutate
mutateExpr :: (Eq i, Eq r, Eq a) => Mutation i -> Mutation r -> Mutation a -> Mutation (Expr i r a)
mutateExpr fi fr fa = mutateWith _msExpr >=> \x -> case x of
  ExprPrim p -> ExprPrim <$> mPrim p
  ExprUnOp op a p -> ExprUnOp op <$> fa a <*> mPrim p
  ExprBinOp l op a r -> flip ExprBinOp op <$> mExpr l <*> fa a <*> mExpr r
  ExprCond c a t f -> ExprCond <$> mExpr c <*> fa a <*> mExpr t <*> mExpr f
  where
    mExpr = mutateExpr fi fr fa
    mPrim = mutatePrim fi fr fa

mutateCExpr :: Mutation CExpr
mutateCExpr (CExpr e) = CExpr <$> mutateExpr pure (traverse mutateCRE) mutateAttr e

mutateNExpr :: Mutation NExpr
mutateNExpr (NExpr e) = NExpr <$> mutateExpr mutateHI (traverse mutateNDR) mutateAttr e

mutateAttr :: Mutation Attributes
mutateAttr = mutateWith _msAttr >=> mutateList >=> mapM (mapM mAttr)
  where
    mAttr (Attribute i v) = Attribute i <$> traverse (mutateExpr pure (traverse mutateCRE) pure) v

mutateAttributed :: Mutation t -> Mutation (Attributed t)
mutateAttributed f (Attributed a x) = Attributed <$> mutateAttr a <*> f x

mutateAttrIded :: Mutation t -> Mutation (AttrIded t)
mutateAttrIded f (AttrIded a i x) = flip AttrIded i <$> mutateAttr a <*> f x

mutateR2 :: Mutation (Range2 CExpr)
mutateR2 (Range2 m l) = Range2 <$> mutateCExpr m <*> mutateCExpr l

mutateRE :: Mutation e -> Mutation (RangeExpr e CExpr)
mutateRE f = mutateWith _msRangeExpr >=> \x -> case x of
  RESingle e -> RESingle <$> f e
  REPair r2 -> REPair <$> mutateR2 r2
  REBaseOff b mp o -> flip REBaseOff mp <$> f b <*> mutateCExpr o

mutateNRE :: Mutation (RangeExpr NExpr CExpr)
mutateNRE = mutateRE mutateNExpr

mutateCRE :: Mutation (RangeExpr CExpr CExpr)
mutateCRE = mutateRE mutateCExpr

mutateD3 :: Mutation (Delay3 NExpr)
mutateD3 = mutateWith _msDelay3 >=> \x -> case x of
  D31 m -> D31 <$> mutateNMTM m
  D32 r f -> D32 <$> mutateNMTM r <*> mutateNMTM f
  D33 r f h -> D33 <$> mutateNMTM r <*> mutateNMTM f <*> mutateNMTM h
  _ -> pure x

mutateD2 :: Mutation (Delay2 NExpr)
mutateD2 = mutateWith _msDelay2 >=> \x -> case x of
  D21 m -> D21 <$> mutateNMTM m
  D22 r f -> D22 <$> mutateNMTM r <*> mutateNMTM f
  _ -> pure x

mutateD1 :: Mutation (Delay1 NExpr)
mutateD1 = mutateWith _msDelay1 >=> \x -> case x of
  D11 m -> D11 <$> mutateNMTM m
  _ -> pure x

mutateSR :: Mutation (SignRange CExpr)
mutateSR (SignRange sn r) = SignRange sn <$> traverse mutateR2 r

mutateST :: Mutation (SpecTerm CExpr)
mutateST (SpecTerm i r) = SpecTerm i <$> traverse mutateCRE r

mutateCT :: Mutation (ComType t CExpr)
mutateCT x = case x of
  CTConcrete e sr -> CTConcrete e <$> mutateSR sr
  _ -> pure x

mutateLV :: Mutation e -> Mutation (LValue e CExpr)
mutateLV f = mutateWith _msLValue >=> \x -> case x of
  LVSingle hi dr -> LVSingle <$> mutateHI hi <*> traverse (mutateDR f) dr
  LVConcat l -> LVConcat <$> mapM (mutateLV f) l

mutateNLV :: Mutation (LValue CExpr CExpr)
mutateNLV = mutateLV mutateCExpr

mutateVLV :: Mutation (LValue NExpr CExpr)
mutateVLV = mutateLV mutateNExpr

mutateAss :: Mutation e -> Mutation (Assign e CExpr NExpr)
mutateAss f = mutateWith _msAssign >=> \(Assign lv e) -> Assign <$> mutateLV f lv <*> mutateNExpr e

mutateNAss :: Mutation (Assign CExpr CExpr NExpr)
mutateNAss = mutateAss mutateCExpr

mutateVAss :: Mutation (Assign NExpr CExpr NExpr)
mutateVAss = mutateAss mutateNExpr

mutateParam :: Mutation (Parameter CExpr)
mutateParam (Parameter t v) = Parameter <$> mutateCT t <*> mutateCMTM v

mutatePO :: Mutation (ParamOver CExpr)
mutatePO (ParamOver hi v) = ParamOver <$> mutateHI hi <*> mutateCMTM v

mutateParamAss :: Mutation (ParamAssign NExpr)
mutateParamAss x = case x of
  ParamPositional l -> ParamPositional <$> mapM mutateNExpr l
  ParamNamed l -> fmap ParamNamed $ mapM (traverse $ traverse mutateNMTM) l >>= mutateList

mutatePortAss :: Mutation (PortAssign NExpr)
mutatePortAss x = case x of
  PortNamed l -> fmap PortNamed $ mapM (mutateAttrIded $ traverse mutateNExpr) l >>= mutateList
  PortPositional l -> PortPositional <$> mapM (mutateAttributed $ traverse mutateNExpr) l

mutateEP :: Mutation (EventPrim NExpr)
mutateEP = mutateWith _msEventPrim >=> \(EventPrim p e) -> EventPrim p <$> mutateNExpr e

mutateEC :: Mutation (EventControl NExpr CExpr)
mutateEC x = case x of
  ECIdent hi -> ECIdent <$> mutateHI hi
  ECExpr l -> fmap ECExpr $ mapM mutateEP l >>= mutateList
  _ -> pure x

mutateDEC :: Mutation (DelayEventControl NExpr CExpr)
mutateDEC x = case x of
  DECDelay d -> DECDelay <$> mutateD1 d
  DECEvent ec -> DECEvent <$> mutateEC ec
  DECRepeat e ec -> DECRepeat <$> mutateNExpr e <*> mutateEC ec

mutatePCA :: Mutation (ProcContAssign NExpr CExpr)
mutatePCA x = case x of
  PCAAssign va -> PCAAssign <$> mutateVAss va
  PCADeassign vlv -> PCADeassign <$> mutateVLV vlv
  PCAForce vana -> PCAForce <$> bitraverse mutateVAss mutateNAss vana
  PCARelease vlvnlv -> PCARelease <$> bitraverse mutateVLV mutateNLV vlvnlv

mutateLS :: Mutation (LoopStatement NExpr CExpr)
mutateLS = mutateWith _msLoopStmt >=> \x -> case x of
  LSRepeat e -> LSRepeat <$> mutateNExpr e
  LSWhile e -> LSWhile <$> mutateNExpr e
  LSFor vi c vu -> LSFor <$> mutateVAss vi <*> mutateNExpr c <*> mutateVAss vu
  _ -> pure x

mutateFStmt :: Mutation (FunctionStatement NExpr CExpr)
mutateFStmt x = do
  y <- maybe x id . fromStatement <$> mutateWith _msStmt (toStatement x)
  case y of
    FSBlockAssign va -> FSBlockAssign <$> mutateVAss va
    FSCase zox e b d ->
      FSCase zox <$> mutateNExpr e
        <*> ( mapM
                ( \(CaseItem pat v) ->
                    CaseItem <$> (mapM mutateNExpr pat >>= mutateList) <*> mutateMFStmt v
                )
                b
                >>= mutateList
            )
        <*> mutateMFStmt d
    FSIf c t f -> FSIf <$> mutateNExpr c <*> mutateMFStmt t <*> mutateMFStmt f
    FSDisable hi -> FSDisable <$> mutateHI hi
    FSLoop ls b -> FSLoop <$> mutateLS ls <*> mutateAFStmt b
    FSBlock h ps b ->
      flip FSBlock ps <$> traverse (traverse $ mapM (mutateAttrIded mutateSBD) >=> mutateList) h
        <*> (mapM mutateAFStmt b >>= if ps then mutateList else return)
  where
    mutateAFStmt = mutateAttributed mutateFStmt
    mutateMFStmt = mutateAttributed $ traverse mutateFStmt

mutateStmt :: Mutation (Statement NExpr CExpr)
mutateStmt = mutateWith _msStmt >=> \x -> case x of
  SBlockAssign b ass dec -> SBlockAssign b <$> mutateVAss ass <*> traverse mutateDEC dec
  SCase zox e b d ->
    SCase zox <$> mutateNExpr e
      <*> ( mapM
              ( \(CaseItem pat v) ->
                  CaseItem <$> (mapM mutateNExpr pat >>= mutateList) <*> mutateMStmt v
              )
              b
              >>= mutateList
          )
      <*> mutateMStmt d
  SIf c t f -> SIf <$> mutateNExpr c <*> mutateMStmt t <*> mutateMStmt f
  SDisable hi -> SDisable <$> mutateHI hi
  SEventTrigger hi e -> SEventTrigger <$> mutateHI hi <*> mapM mutateNExpr e
  SLoop ls b -> SLoop <$> mutateLS ls <*> mutateAStmt b
  SProcContAssign pca -> SProcContAssign <$> mutatePCA pca
  SProcTimingControl tec s ->
    SProcTimingControl <$> bitraverse mutateD1 mutateEC tec <*> mutateMStmt s
  SBlock h ps b ->
    flip SBlock ps <$> traverse (traverse $ mapM (mutateAttrIded mutateSBD) >=> mutateList) h
      <*> (mapM mutateAStmt b >>= if ps then mutateList else return)
  SSysTaskEnable i args -> SSysTaskEnable i <$> mapM (traverse mutateNExpr) args
  STaskEnable hi args -> STaskEnable <$> mutateHI hi <*> mapM mutateNExpr args
  SWait e s -> SWait <$> mutateNExpr e <*> mutateMStmt s

mutateAStmt :: Mutation (Attributed (Statement NExpr CExpr))
mutateAStmt = mutateAttributed mutateStmt

mutateMStmt :: Mutation (Attributed (Maybe (Statement NExpr CExpr)))
mutateMStmt = mutateAttributed $ traverse mutateStmt

mutateNP :: Mutation (NetProp NExpr CExpr)
mutateNP (NetProp sn v d) = NetProp sn <$> traverse (traverse mutateR2) v <*> traverse mutateD3 d

mutateND :: Mutation (NetDecl CExpr)
mutateND (NetDecl i r2) = NetDecl i <$> mapM mutateR2 r2

mutateNI :: Mutation (NetInit NExpr)
mutateNI (NetInit i e) = NetInit i <$> mutateNExpr e

mutateBD :: (forall x. Mutation x -> Mutation (f x)) -> Mutation t -> Mutation (BlockDecl f t CExpr)
mutateBD ff ft x = case x of
  BDReg sr d -> BDReg <$> mutateSR sr <*> ff ft d
  BDInt d -> BDInt <$> ff ft d
  BDReal d -> BDReal <$> ff ft d
  BDTime d -> BDTime <$> ff ft d
  BDRealTime d -> BDRealTime <$> ff ft d
  BDEvent d -> BDEvent <$> ff (mapM mutateR2) d
  BDLocalParam ct v -> BDLocalParam <$> mutateCT ct <*> ff mutateCMTM v

mutateSBD :: Mutation (StdBlockDecl CExpr)
mutateSBD x = case x of
  SBDBlockDecl bd -> SBDBlockDecl <$> mutateBD traverse (mapM mutateR2) bd
  SBDParameter p -> SBDParameter <$> mutateParam p

mutateTFBD :: Mutation (TFBlockDecl t CExpr)
mutateTFBD x = case x of
  TFBDStd sbd -> TFBDStd <$> mutateSBD sbd
  TFBDPort d t -> TFBDPort d <$> mutateCT t

mutateGCI :: Mutation (GenCaseItem NExpr CExpr)
mutateGCI (GenCaseItem pat v) =
  GenCaseItem <$> (mapM mutateCExpr pat >>= mutateList) <*> mutateGCB v

mutateMGCI :: Mutation (ModGenCondItem NExpr CExpr)
mutateMGCI = mutateWith _msGenCond >=> \x -> case x of
  MGCIIf c t f -> MGCIIf <$> mutateCExpr c <*> mutateGCB t <*> mutateGCB f
  MGCICase e b d -> MGCICase <$> mutateCExpr e <*> (mapM mutateGCI b >>= mutateList) <*> mutateGCB d

mutateGCB :: Mutation (GenerateCondBlock NExpr CExpr)
mutateGCB x = case x of
  GCBBlock b -> GCBBlock <$> mutateGB b
  GCBConditional c -> GCBConditional <$> mutateAttributed mutateMGCI c
  _ -> pure x

mutateInstanceName :: Mutation (InstanceName CExpr)
mutateInstanceName (InstanceName i r2) = InstanceName i <$> traverse mutateR2 r2

mutateGICMos :: Mutation (GICMos NExpr CExpr)
mutateGICMos (GICMos i lv inp nc pc) =
  GICMos <$> traverse mutateInstanceName i
    <*> mutateNLV lv
    <*> mutateNExpr inp
    <*> mutateNExpr nc
    <*> mutateNExpr pc

mutateGIEnable :: Mutation (GIEnable NExpr CExpr)
mutateGIEnable (GIEnable i lv inp en) =
  GIEnable <$> traverse mutateInstanceName i <*> mutateNLV lv <*> mutateNExpr inp <*> mutateNExpr en

mutateGIMos :: Mutation (GIMos NExpr CExpr)
mutateGIMos (GIMos i lv inp en) =
  GIMos <$> traverse mutateInstanceName i <*> mutateNLV lv <*> mutateNExpr inp <*> mutateNExpr en

mutateGINIn :: Mutation (GINIn NExpr CExpr)
mutateGINIn (GINIn i lv inp) =
  GINIn <$> traverse mutateInstanceName i <*> mutateNLV lv <*> (mapM mutateNExpr inp >>= mutateList)

mutateGINOut :: Mutation (GINOut NExpr CExpr)
mutateGINOut (GINOut i lv inp) =
  GINOut <$> traverse mutateInstanceName i <*> (mapM mutateNLV lv >>= mutateList) <*> mutateNExpr inp

mutateGIPassEn :: Mutation (GIPassEn NExpr CExpr)
mutateGIPassEn (GIPassEn i lhs rhs en) =
  GIPassEn <$> traverse mutateInstanceName i <*> mutateNLV lhs <*> mutateNLV rhs <*> mutateNExpr en

mutateGIPass :: Mutation (GIPass NExpr CExpr)
mutateGIPass (GIPass i lhs rhs) =
  GIPass <$> traverse mutateInstanceName i <*> mutateNLV lhs <*> mutateNLV rhs

mutateGIPull :: Mutation (GIPull NExpr CExpr)
mutateGIPull (GIPull i lv) = GIPull <$> traverse mutateInstanceName i <*> mutateNLV lv

mutateUDPInst :: Mutation (UDPInst NExpr CExpr)
mutateUDPInst (UDPInst i lv args) =
  UDPInst <$> traverse mutateInstanceName i <*> mutateNLV lv <*> mapM mutateNExpr args

mutateModInst :: Mutation (ModInst NExpr CExpr)
mutateModInst (ModInst i ports) = ModInst <$> mutateInstanceName i <*> mutatePortAss ports

mutateUknInst :: Mutation (UknInst NExpr CExpr)
mutateUknInst (UknInst i a0 args) =
  UknInst <$> mutateInstanceName i <*> mutateNLV a0 <*> mapM mutateNExpr args

mutateGate :: Mutation (Gate Identity NExpr CExpr)
mutateGate x = case x of
  GCMos r d3 i -> GCMos r <$> traverse mutateD3 d3 <*> traverse mutateGICMos i
  GEnable r b ds d3 i -> GEnable r b ds <$> traverse mutateD3 d3 <*> traverse mutateGIEnable i
  GMos r np d3 i -> GMos r np <$> traverse mutateD3 d3 <*> traverse mutateGIMos i
  GNIn nin n ds d2 i -> GNIn nin n ds <$> traverse mutateD2 d2 <*> traverse mutateGINIn i
  GNOut r ds d2 i -> GNOut r ds <$> traverse mutateD2 d2 <*> traverse mutateGINOut i
  GPassEn r b d2 i -> GPassEn r b <$> traverse mutateD2 d2 <*> traverse mutateGIPassEn i
  GPass r i -> GPass r <$> traverse mutateGIPass i
  GPull ud ds i -> GPull ud ds <$> traverse mutateGIPull i

mutateMGI :: Mutation (ModGenBlockedItem NExpr CExpr)
mutateMGI = mutateWith _msModGenItem >=> \x -> case x of
  MGINetInit nt ds np ni -> MGINetInit nt ds <$> mutateNP np <*> traverse mutateNI ni
  MGINetDecl nt np nd -> MGINetDecl nt <$> mutateNP np <*> traverse mutateND nd
  MGITriD ds np ni -> MGITriD ds <$> mutateNP np <*> traverse mutateNI ni
  MGITriC cs np nd -> MGITriC cs <$> mutateNP np <*> traverse mutateND nd
  MGIBlockDecl bd ->
    MGIBlockDecl <$> mutateBD traverse (bitraverse (mapM mutateR2) mutateCExpr) bd
  MGITask a i d b ->
    MGITask a i <$> (mapM (mutateAttrIded mutateTFBD) d >>= mutateList) <*> mutateMStmt b
  MGIFunc a t i d b ->
    flip (MGIFunc a) i <$> traverse mutateCT t
      <*> (mapM (mutateAttrIded mutateTFBD) d >>= mutateList)
      <*> mutateFStmt b
  MGIDefParam po -> MGIDefParam <$> traverse mutatePO po
  MGIContAss ds d3 na -> MGIContAss ds <$> traverse mutateD3 d3 <*> traverse mutateNAss na
  MGIGate g -> MGIGate <$> mutateGate g
  MGIUDPInst kind ds d2 i ->
    MGIUDPInst kind ds <$> traverse mutateD2 d2 <*> traverse mutateUDPInst i
  MGIModInst kind params i ->
    MGIModInst kind <$> mutateParamAss params <*> traverse mutateModInst i
  MGIUnknownInst kind params i ->
    MGIUnknownInst kind
      <$> traverse (bitraverse mutateNExpr $ bitraverse mutateNExpr mutateNExpr) params
      <*> traverse mutateUknInst i
  MGIInitial s -> MGIInitial <$> mutateAStmt s
  MGIAlways s -> MGIAlways <$> mutateAStmt s
  MGILoopGen ii iv c ui uv b ->
    MGILoopGen ii <$> mutateCExpr iv
      <*> mutateCExpr c
      <*> pure ui
      <*> mutateCExpr uv
      <*> mutateGB b
  MGICondItem ci -> MGICondItem <$> mutateMGCI ci
  _ -> pure x

mutateTCE :: Mutation (TimingCheckEvent NExpr CExpr)
mutateTCE (TimingCheckEvent ec st tcc) =
  TimingCheckEvent ec <$> mutateST st <*> traverse (traverse mutateNExpr) tcc

mutateCTCE :: Mutation (ControlledTimingCheckEvent NExpr CExpr)
mutateCTCE (ControlledTimingCheckEvent ec st tcc) =
  ControlledTimingCheckEvent ec <$> mutateST st <*> traverse (traverse mutateNExpr) tcc

mutateSTCA :: Mutation (STCArgs NExpr CExpr)
mutateSTCA (STCArgs de re tcl n) =
  STCArgs <$> mutateTCE de <*> mutateTCE re <*> mutateNExpr tcl <*> pure n

mutateSTCAA :: Mutation (STCAddArgs NExpr CExpr)
mutateSTCAA (STCAddArgs tcl sc ctc dr dd) =
  STCAddArgs <$> mutateNExpr tcl
    <*> traverse mutateNMTM sc
    <*> traverse mutateNMTM ctc
    <*> traverse (traverse $ traverse mutateCMTM) dr
    <*> traverse (traverse $ traverse mutateCMTM) dd

mutateMPC :: Mutation (ModulePathCondition MPExpr)
mutateMPC x = case x of
  MPCCond e -> MPCCond <$> mutateExpr pure pure mutateAttr e
  _ -> pure x

mutateSP :: Mutation (SpecPath CExpr)
mutateSP x = case x of
  SPParallel inp outp -> SPParallel <$> mutateST inp <*> mutateST outp
  SPFull inp outp ->
    SPFull <$> (mapM mutateST inp >>= mutateList) <*> (mapM mutateST outp >>= mutateList)

mutatePDV :: Mutation (PathDelayValue CExpr)
mutatePDV = mutateWith _msPDV >=> \x -> case x of
  PDV1 x -> PDV1 <$> mutateCMTM x
  PDV2 r f -> PDV2 <$> mutateCMTM r <*> mutateCMTM f
  PDV3 r f z -> PDV3 <$> mutateCMTM r <*> mutateCMTM f <*> mutateCMTM z
  PDV6 t01 t10 t0z tz1 t1z tz0 ->
    PDV6 <$> mutateCMTM t01
      <*> mutateCMTM t10
      <*> mutateCMTM t0z
      <*> mutateCMTM tz1
      <*> mutateCMTM t1z
      <*> mutateCMTM tz0
  PDV12 t01 t10 t0z tz1 t1z tz0 t0x tx1 t1x tx0 txz tzx ->
    PDV12 <$> mutateCMTM t01
      <*> mutateCMTM t10
      <*> mutateCMTM t0z
      <*> mutateCMTM tz1
      <*> mutateCMTM t1z
      <*> mutateCMTM tz0
      <*> mutateCMTM t0x
      <*> mutateCMTM tx1
      <*> mutateCMTM t1x
      <*> mutateCMTM tx0
      <*> mutateCMTM txz
      <*> mutateCMTM tzx

mutateSI :: Mutation (SpecifyBlockedItem NExpr CExpr MPExpr)
mutateSI = mutateWith _msSpecItem >=> \x -> case x of
  SISpecParam r2 spd -> SISpecParam <$> traverse mutateR2 r2 <*> traverse mutateSPD spd
  SIPulsestyleOnevent st -> SIPulsestyleOnevent <$> traverse mutateST st
  SIPulsestyleOndetect st -> SIPulsestyleOndetect <$> traverse mutateST st
  SIShowcancelled st -> SIShowcancelled <$> traverse mutateST st
  SINoshowcancelled st -> SINoshowcancelled <$> traverse mutateST st
  SIPathDeclaration mpc con pol eds v ->
    SIPathDeclaration <$> mutateMPC mpc
      <*> mutateSP con
      <*> pure pol
      <*> traverse (bitraverse mutateNExpr pure) eds
      <*> mutatePDV v
  SISetup a -> SISetup <$> mutateSTCA a
  SIHold a -> SIHold <$> mutateSTCA a
  SISetupHold a aa -> SISetupHold <$> mutateSTCA a <*> mutateSTCAA aa
  SIRecovery a -> SIRecovery <$> mutateSTCA a
  SIRemoval a -> SIRemoval <$> mutateSTCA a
  SIRecrem a aa -> SIRecrem <$> mutateSTCA a <*> mutateSTCAA aa
  SISkew a -> SISkew <$> mutateSTCA a
  SITimeSkew a eb ra ->
    SITimeSkew <$> mutateSTCA a <*> traverse mutateCExpr eb <*> traverse mutateCExpr ra
  SIFullSkew a tcl eb ra ->
    SIFullSkew <$> mutateSTCA a
      <*> mutateNExpr tcl
      <*> traverse mutateCExpr eb
      <*> traverse mutateCExpr ra
  SIPeriod ctce tcl n -> SIPeriod <$> mutateCTCE ctce <*> mutateNExpr tcl <*> pure n
  SIWidth ctce tcl t n ->
    SIWidth <$> mutateCTCE ctce <*> mutateNExpr tcl <*> traverse mutateCExpr t <*> pure n
  SINoChange re de se ee n ->
    SINoChange <$> mutateTCE re <*> mutateTCE de <*> mutateNMTM se <*> mutateNMTM ee <*> pure n

mutateSPD :: Mutation (SpecParamDecl CExpr)
mutateSPD x = case x of
  SPDAssign i v -> SPDAssign i <$> mutateCMTM v
  SPDPathPulse io rej err ->
    SPDPathPulse <$> traverse (bitraverse mutateST mutateST) io
      <*> mutateCMTM rej
      <*> mutateCMTM err

mutateMI :: Mutation (ModuleItem NExpr CExpr MPExpr)
mutateMI x = do
  y <- mutateWith _msModItem x
  case y of
    MIMGI mgi -> MIMGI <$> mutateAttributed mutateMGI mgi
    MIPort p -> MIPort <$> mutateAttrIded (traverse mutateSR) p
    MIParameter p -> MIParameter <$> mutateAttrIded mutateParam p
    MIGenReg l -> MIGenReg <$> mapM (mutateAttributed mutateMGI) l
    MISpecParam a r2 spd ->
      MISpecParam <$> mutateAttr a <*> traverse mutateR2 r2 <*> mutateSPD spd
    MISpecBlock l -> fmap MISpecBlock $ mapM mutateSI l >>= mutateList

mutateGB :: Mutation (GenerateBlock NExpr CExpr)
mutateGB =
  mutateWith _msGenBlk
    >=> \(GenerateBlock i b) -> GenerateBlock i <$> mapM (mutateAttributed mutateMGI) b

mutateMB :: Mutation (ModuleBlock NExpr CExpr MPExpr)
mutateMB = mutateWith _msModule >=> \(ModuleBlock a b i pi mi ts c p dnt) ->
  (\a pi mi -> ModuleBlock a b i pi mi ts c p dnt) <$> mutateAttr a
    <*> mapM (traverse $ mapM $ traverse $ traverse mutateCRE) pi
    <*> mapM mutateMI mi

mutatePT :: Mutation PrimTable
mutatePT = mutateWith _msPrimTable >=> \x -> case x of
  CombTable l -> CombTable <$> mutateList l
  SeqTable i l -> SeqTable i <$> mutateList l

mutatePP :: Mutation (PrimPort CExpr)
mutatePP x = case x of
  PPOutReg e -> PPOutReg <$> traverse mutateCExpr e
  _ -> pure x

mutatePB :: Mutation (PrimitiveBlock CExpr)
mutatePB = mutateWith _msPrimitive >=> \(PrimitiveBlock a i outp inp pd b) ->
  (\a b c -> PrimitiveBlock a i outp inp b c) <$> mutateAttr a
    <*> mapM (mutateAttrIded mutatePP) pd
    <*> mutatePT b

mutateCB :: Mutation ConfigBlock
mutateCB (ConfigBlock i de b dft) = fmap (flip (ConfigBlock i de) dft) $ mapM pure b >>= mutateList

mutateV2005 :: Mutation Verilog2005
mutateV2005 = mutateWith _msV2005 >=> \(Verilog2005 m p c) ->
  Verilog2005 <$> mapM mutateMB m
    <*> (mapM mutatePB p >>= mutateList)
    <*> (mapM mutateCB c >>= mutateList)

runMutation :: Config -> Verilog2005 -> IO Verilog2005
runMutation c v = do
  let conf = _configMutation c
  gen <- maybe createSystemRandom initialize $ _moSeed conf
  runReaderT (mutateV2005 v) (buildStore conf, gen)

-- Actual mutations

shuffleList :: IsList t => PrimMutation t
shuffleList l = Just $ asks snd >>= \gen -> fromList <$> shuffle gen (toList l)

mtmTo3 :: PureMutation (MinTypMax t)
mtmTo3 x = case x of
  MTMSingle e -> Just $ MTMFull e e e
  _ -> Nothing

mtmTo1 :: Eq t => PureMutation (MinTypMax t)
mtmTo1 x = case x of
  MTMFull e0 e1 e2 | e0 == e1 && e1 == e2 -> Just $ MTMSingle e0
  _ -> Nothing

valueToNumIdent :: EE.Value -> Maybe NumIdent
valueToNumIdent v = case v of
  EE.VInt sn sz v 0 -> let n = fitSnSz sn sz v in
    if n >= 0 then Just $ NINumber $ fromInteger n else Nothing
  EE.VReal r -> Just $ NIReal $ packChars $ show r
  _ -> Nothing

nexprToNumIdent :: NExpr -> Maybe NumIdent
nexprToNumIdent (NExpr e) = case e of
  ExprPrim (PrimIdent (HierIdent [] i) Nothing) -> Just $ NIIdent i
  ExprPrim (PrimMinTypMax (MTMSingle e)) -> nexprToNumIdent $ NExpr e
  _ -> EE.evalNExprSelfDet (NExpr e) >>= valueToNumIdent

numIdentToNExpr :: NumIdent -> NExpr
numIdentToNExpr ni = NExpr $ ExprPrim $ case ni of
  NIReal r -> PrimReal r
  NINumber n -> PrimNumber 0 False $ NDecimal n
  NIIdent i -> PrimIdent (HierIdent [] i) Nothing

delay1ToBase :: PureMutation (Delay1 NExpr)
delay1ToBase x = case x of
  D11 (MTMSingle e) -> D1Base <$> nexprToNumIdent e
  _ -> Nothing

delay1To1 :: PureMutation (Delay1 NExpr)
delay1To1 x = case x of
  D1Base ni -> Just $ D11 $ MTMSingle $ numIdentToNExpr ni
  _ -> Nothing

delay2ToBase :: PureMutation (Delay2 NExpr)
delay2ToBase x = case x of
  D21 (MTMSingle e) -> D2Base <$> nexprToNumIdent e
  D22 (MTMSingle e1) (MTMSingle e2) | e1 == e2 -> D2Base <$> nexprToNumIdent e1
  _ -> Nothing

delay2To1 :: PureMutation (Delay2 NExpr)
delay2To1 x = case x of
  D2Base ni -> Just $ D21 $ MTMSingle $ numIdentToNExpr ni
  D22 mtm1 mtm2 | mtm1 == mtm2 -> Just $ D21 mtm1
  _ -> Nothing

delay2To2 :: PureMutation (Delay2 NExpr)
delay2To2 x = case x of
  D2Base ni -> Just $ let x = MTMSingle $ numIdentToNExpr ni in D22 x x
  D21 mtm -> Just $ D22 mtm mtm
  _ -> Nothing

delay3ToBase :: PureMutation (Delay3 NExpr)
delay3ToBase x = case x of
  D31 (MTMSingle e) -> D3Base <$> nexprToNumIdent e
  D32 (MTMSingle e1) (MTMSingle e2) | e1 == e2 -> D3Base <$> nexprToNumIdent e1
  D33 (MTMSingle e1) (MTMSingle e2) (MTMSingle e3) | e1 == e2 && e2 == e3 ->
    D3Base <$> nexprToNumIdent e1
  _ -> Nothing

delay3To1 :: PureMutation (Delay3 NExpr)
delay3To1 x = case x of
  D3Base ni -> Just $ D31 $ MTMSingle $ numIdentToNExpr ni
  D32 mtm1 mtm2 | mtm1 == mtm2 -> Just $ D31 mtm1
  D33 mtm1 mtm2 mtm3 | mtm1 == mtm2 && mtm2 == mtm3 -> Just $ D31 mtm1
  _ -> Nothing

delay3To2 :: PureMutation (Delay3 NExpr)
delay3To2 x = case x of
  D3Base ni -> Just $ let x = MTMSingle $ numIdentToNExpr ni in D32 x x
  D31 mtm -> Just $ D32 mtm mtm
  D33 mtm1 mtm2 mtm3 | mtm1 == mtm2 && mtm2 == mtm3 -> Just $ D32 mtm1 mtm1
  _ -> Nothing

delay3To3 :: PureMutation (Delay3 NExpr)
delay3To3 x = case x of
  D3Base ni -> Just $ let x = MTMSingle $ numIdentToNExpr ni in D33 x x x
  D31 mtm -> Just $ D33 mtm mtm mtm
  D32 mtm1 mtm2 | mtm1 == mtm2 -> Just $ D33 mtm1 mtm1 mtm1
  _ -> Nothing

loopForever :: PureMutation (LoopStatement NExpr CExpr)
loopForever x = case x of
  LSWhile e | fmap EE.evalTruth (EE.evalNExprSelfDet e) == Just ZOXO -> Just $ LSForever
  _ -> Nothing

loopRepeat :: PrimMutation (LoopStatement NExpr CExpr)
loopRepeat x = case x of
  LSWhile e | maybe False (/= ZOXO) (EE.evalTruth <$> EE.evalNExprSelfDet e) -> Just $ do
    -- TODO: pull random for e OR 0 OR Z OR X
    return $ LSRepeat e
  _ -> Nothing

loopWhile :: PrimMutation (LoopStatement NExpr CExpr)
loopWhile x = case x of
  LSForever -> Just $ do
    -- TODO: pull random for non 0 NOR Z NOR X
    return $ LSWhile $ NExpr $ ExprPrim $ PrimNumber 0 False $ NDecimal 1
  LSRepeat e | maybe False (/= ZOXO) (EE.evalTruth <$> EE.evalNExprSelfDet e) -> Just $ do
    -- TODO: pull random for e OR 0 OR Z OR X
    return $ LSWhile e
  _ -> Nothing

-- loopSForever :: PureMutation Statement
-- loopSRepeat :: PureMutation Statement
-- loopSWhile :: PureMutation Statement
-- loopSFor :: PureMutation Statement
-- loopFSForever :: PureMutation FStatement
-- loopFSRepeat :: PureMutation FStatement
-- loopFSWhile :: PureMutation FStatement
-- loopFSFor :: PureMutation FStatement

-- constToBin :: PureMutation (GenPrim i r a)
-- constToOct :: PureMutation (GenPrim i r a)
-- constToHex :: PureMutation (GenPrim i r a)
-- constToDec :: PureMutation (GenPrim i r a)
-- constToXZ :: PureMutation (GenPrim i r a)
-- constToStr :: PureMutation (GenPrim i r a)
-- constSplit :: PrimMutation (GenPrim i r a)
-- commuteRel :: PureMutation (GenExpr i r a)
-- reshapeCommAssoc :: PrimMutation (GenExpr i r a)
