-- Module      : Verismith.Verilog2005.Utils
-- Description : AST utilitary functions.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

{-# LANGUAGE OverloadedLists #-}

module Verismith.Verilog2005.Utils
  ( makeIdent,
    regroup,
    addAttributed,
    genexprnumber,
    constifyIdent,
    constifyMaybeRange,
    trConstifyExpr,
    constifyExpr,
    constifyLV,
    expr2netlv,
    netlv2expr,
    toStatement,
    fromStatement,
    fromMybStmt,
    toMGIBlockDecl,
    fromMGIBlockDecl1,
    fromMGIBlockDecl_add,
    toStdBlockDecl,
    toSpecBlockedItem,
    fromSpecBlockedItem,
    toMGBlockedItem,
    fromMGBlockedItem1,
    fromMGBlockedItem_add,
    fromMGBlockedItem,
    resolveInsts,
    fitSnSz,
  )
where

import Control.Lens ((%~))
import Data.Data.Lens (biplate)
import Numeric.Natural
import Text.Printf (printf)
import Data.Bits
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Function (on, (&))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w, packChars)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty (..), (<|), toList)
import qualified Data.List.NonEmpty as NE
import Verismith.Verilog2005.Lexer (VerilogVersion (..), isIdentSimple)
import Verismith.Verilog2005.AST
import Verismith.Utils (nonEmpty, foldrMap1)

-- AST utils

makeIdent :: BS.ByteString -> Identifier
makeIdent =
  Identifier . BS.concatMap
    (\w -> if 33 <= w && w <= 126 then BS.pack [w] else packChars $ printf "\\%02x" w)

-- | Groups `x`s into `y`s by converting a single `x` and merging previous `x`s to the result
regroup :: (x -> y) -> (x -> y -> Maybe y) -> NonEmpty x -> NonEmpty y
regroup mk add = foldrMap1 ((:|[]) . mk) (\e (h :| t) -> maybe (mk e :| h : t) (:| t) $ add e h)

-- | Merges `Attributed x`s if we can merge `x`s
addAttributed :: (x -> y -> Maybe y) -> Attributed x -> Attributed y -> Maybe (Attributed y)
addAttributed f (Attributed na x) (Attributed a y) =
  if a /= na then Nothing else Attributed a <$> f x y

-- | Makes a Verilog2005 expression out of a number
genexprnumber :: Natural -> Expr i r a
genexprnumber = ExprPrim . PrimNumber 0 False . NDecimal

-- | converts HierIdent into Identifier
constifyIdent :: HierIdent ce -> Maybe Identifier
constifyIdent (HierIdent p i) = case p of [] -> Just i; _ -> Nothing

-- | the other way
unconstIdent :: Identifier -> HierIdent ce
unconstIdent = HierIdent []

-- | converts NPrim into Prim i r
constifyPrim ::
  (si -> Maybe di) ->
  (Maybe (DimRange NExpr CExpr) -> Maybe r) ->
  Prim si (Maybe (DimRange NExpr CExpr)) a ->
  Maybe (Prim di r a)
constifyPrim fi fr x = case x of
  PrimNumber s b n -> Just $ PrimNumber s b n
  PrimReal s -> Just $ PrimReal s
  PrimIdent s rng -> PrimIdent <$> fi s <*> fr rng
  PrimConcat e -> PrimConcat <$> mapM ce e
  PrimMultConcat m e -> PrimMultConcat m <$> mapM ce e
  PrimFun s a e -> PrimFun <$> fi s <*> pure a <*> mapM ce e
  PrimSysFun s e -> PrimSysFun s <$> mapM ce e
  PrimMinTypMax (MTMSingle e) -> PrimMinTypMax . MTMSingle <$> ce e
  PrimMinTypMax (MTMFull l t h) -> PrimMinTypMax <$> (MTMFull <$> ce l <*> ce t <*> ce h)
  PrimString s -> Just $ PrimString s
  where
    ce = trConstifyExpr fi fr

-- | the other way
unconstPrim ::
  Prim Identifier (Maybe (RangeExpr CExpr CExpr)) a ->
  Prim (HierIdent CExpr) (Maybe (DimRange NExpr CExpr)) a
unconstPrim x = case x of
  PrimNumber s b n -> PrimNumber s b n
  PrimReal s -> PrimReal s
  PrimIdent s rng -> PrimIdent (unconstIdent s) (DimRange [] . unconstRange <$> rng)
  PrimConcat e -> PrimConcat (NE.map trUnconstExpr e)
  PrimMultConcat m e -> PrimMultConcat m (NE.map trUnconstExpr e)
  PrimFun s a e -> PrimFun (unconstIdent s) a (map trUnconstExpr e)
  PrimSysFun s e -> PrimSysFun s (map trUnconstExpr e)
  PrimMinTypMax (MTMSingle e) -> PrimMinTypMax (MTMSingle (trUnconstExpr e))
  PrimMinTypMax (MTMFull l t h) ->
    PrimMinTypMax $ MTMFull (trUnconstExpr l) (trUnconstExpr t) (trUnconstExpr h)
  PrimString s -> PrimString s

-- | converts `Expr si (MaybeDimRange) a` into `Expr i r a`
trConstifyExpr ::
  (si -> Maybe di) ->
  (Maybe (DimRange NExpr CExpr) -> Maybe r) ->
  Expr si (Maybe (DimRange NExpr CExpr)) a ->
  Maybe (Expr di r a)
trConstifyExpr fi fr x = case x of
  ExprPrim p -> ExprPrim <$> constifyPrim fi fr p
  ExprUnOp op a p -> ExprUnOp op a <$> constifyPrim fi fr p
  ExprBinOp lhs op a rhs -> ExprBinOp <$> ce lhs <*> pure op <*> pure a <*> ce rhs
  ExprCond c a t f -> ExprCond <$> ce c <*> pure a <*> ce t <*> ce f
  where
    ce = trConstifyExpr fi fr

-- | converts NExpr's `DimRange` into CExpr's `RangeExpr`
constifyMaybeRange :: Maybe (DimRange NExpr CExpr) -> Maybe (Maybe (RangeExpr CExpr CExpr))
constifyMaybeRange =
  maybe (Just Nothing) $ \(DimRange l r) -> if null l then Just <$> constifyRange r else Nothing

-- | converts NExpr into CExpr
trConstifyNExpr ::
  Expr (HierIdent CExpr) (Maybe (DimRange NExpr CExpr)) a ->
  Maybe (Expr Identifier (Maybe (RangeExpr CExpr CExpr)) a)
trConstifyNExpr = trConstifyExpr constifyIdent constifyMaybeRange

-- | the other way
trUnconstExpr ::
  Expr Identifier (Maybe (RangeExpr CExpr CExpr)) a ->
  Expr (HierIdent CExpr) (Maybe (DimRange NExpr CExpr)) a
trUnconstExpr x = case x of
  ExprPrim p -> ExprPrim (unconstPrim p)
  ExprUnOp op a p -> ExprUnOp op a (unconstPrim p)
  ExprBinOp lhs op a rhs -> ExprBinOp (trUnconstExpr lhs) op a (trUnconstExpr rhs)
  ExprCond c a t f -> ExprCond (trUnconstExpr c) a (trUnconstExpr t) (trUnconstExpr f)

-- | converts NExpr to CExpr
constifyExpr :: NExpr -> Maybe CExpr
constifyExpr (NExpr e) = CExpr <$> trConstifyNExpr e

-- | the other way
unconstExpr :: CExpr -> NExpr
unconstExpr (CExpr e) = NExpr (trUnconstExpr e)

-- | converts RangeExpr
constifyRange :: RangeExpr NExpr CExpr -> Maybe (RangeExpr CExpr CExpr)
constifyRange x = case x of
  RESingle e -> RESingle <$> constifyExpr e
  REPair r2 -> Just $ REPair r2
  REBaseOff b mp o -> (\cb -> REBaseOff cb mp o) <$> constifyExpr b

-- | the other way
unconstRange :: RangeExpr CExpr CExpr -> RangeExpr NExpr CExpr
unconstRange x = case x of
  RESingle e -> RESingle (unconstExpr e)
  REPair r2 -> REPair r2
  REBaseOff b mp o -> REBaseOff (unconstExpr b) mp o

-- | converts DimRange
constifyDR :: DimRange NExpr CExpr -> Maybe (DimRange CExpr CExpr)
constifyDR (DimRange dim rng) = DimRange <$> mapM constifyExpr dim <*> constifyRange rng

-- | the other way
unconstDR :: DimRange CExpr CExpr -> DimRange NExpr CExpr
unconstDR (DimRange dim rng) = DimRange (map unconstExpr dim) (unconstRange rng)

-- | converts variable lvalue into net lvalue
constifyLV :: LValue NExpr CExpr -> Maybe (LValue CExpr CExpr)
constifyLV v = case v of
  LVSingle hi mdr -> LVSingle hi <$> maybe (Just Nothing) (fmap Just . constifyDR) mdr
  LVConcat l -> LVConcat <$> mapM constifyLV l

-- | the other way
unconstLV :: LValue CExpr CExpr -> LValue NExpr CExpr
unconstLV n = case n of
  LVSingle hi dr -> LVSingle hi (unconstDR <$> dr)
  LVConcat l -> LVConcat (NE.map unconstLV l)

-- | converts expression into net lvalue
expr2netlv :: NExpr -> Maybe (LValue CExpr CExpr)
expr2netlv (NExpr x) = aux x
  where
    aux x = case x of
      ExprPrim (PrimConcat c) -> LVConcat <$> mapM aux c
      ExprPrim (PrimIdent s sub) -> LVSingle s <$> maybe (Just Nothing) (Just . constifyDR) sub
      _ -> Nothing

-- | the other way
netlv2expr :: LValue CExpr CExpr -> NExpr
netlv2expr = NExpr . aux
  where
    aux x = case x of
      LVConcat e -> ExprPrim $ PrimConcat $ NE.map aux e
      LVSingle s dr -> ExprPrim $ PrimIdent s $ unconstDR <$> dr

-- | Converts Function statements to statements
toStatement :: FunctionStatement e ce -> Statement e ce
toStatement x = case x of
  FSBlockAssign va -> SBlockAssign True va Nothing
  FSCase zox e l d -> SCase zox e (map (\(CaseItem p v) -> CaseItem p $ mybf v) l) (mybf d)
  FSIf e t f -> SIf e (mybf t) (mybf f)
  FSDisable hi -> SDisable hi
  FSLoop ls b -> SLoop ls $ toStatement <$> b
  FSBlock h ps b -> SBlock h ps $ fmap toStatement <$> b
  where mybf = fmap $ fmap toStatement

-- | the other way
fromStatement :: Statement e ce -> Maybe (FunctionStatement e ce)
fromStatement x = case x of
  SBlockAssign True va Nothing -> Just $ FSBlockAssign va
  SCase zox e l d -> FSCase zox e <$> traverse (\(CaseItem p v) -> CaseItem p <$> mybf v) l <*> mybf d
  SIf e t f -> FSIf e <$> mybf t <*> mybf f
  SDisable hi -> Just $ FSDisable hi
  SLoop ls b -> FSLoop ls <$> traverse fromStatement b
  SBlock h ps b -> FSBlock h ps <$> traverse (traverse fromStatement) b
  _ -> Nothing
  where mybf s = traverse (traverse fromStatement) s

-- | Converts MybStmt to AttrStmt
fromMybStmt :: Attributed (Maybe (Statement NExpr CExpr)) -> Attributed (Statement NExpr CExpr)
fromMybStmt x = case x of
  Attributed _ Nothing -> Attributed [] $ SIf (NExpr $ genexprnumber 1) x $ Attributed [] Nothing
  Attributed a (Just s) -> Attributed a s

type BD f t = BlockDecl (Compose f Identified) t CExpr

-- | Converts ModGenSingleItem's `BlockDecl` into ModGenBlockedItem's `BlockDecl`
toMGIBlockDecl :: BD NonEmpty t -> NonEmpty (BD Identity t)
toMGIBlockDecl x = case x of
  BDReg sr d -> conv (BDReg sr) d
  BDInt d -> conv BDInt d
  BDReal d -> conv BDReal d
  BDTime d -> conv BDTime d
  BDRealTime d -> conv BDRealTime d
  BDEvent d -> conv BDEvent d
  BDLocalParam t d -> conv (BDLocalParam t) d
  where conv f = fmap (f . Compose . Identity) . getCompose

-- | Converts one ModGenBlockedItem's `BlockDecl` into ModGenSingleItem's `BlockDecl`
fromMGIBlockDecl1 :: BD Identity t -> BD NonEmpty t
fromMGIBlockDecl1 x = case x of
  BDReg sr d -> conv (BDReg sr) d
  BDInt d -> conv BDInt d
  BDReal d -> conv BDReal d
  BDTime d -> conv BDTime d
  BDRealTime d -> conv BDRealTime d
  BDEvent d -> conv BDEvent d
  BDLocalParam t d -> conv (BDLocalParam t) d
  where conv f = f . Compose . (:|[]) . runIdentity . getCompose

-- | Merges one ModGenBlockedItem's `BlockDecl` with one ModGenSingleItem's `BlockDecl`
fromMGIBlockDecl_add :: BD Identity t -> BD NonEmpty t -> Maybe (BD NonEmpty t)
fromMGIBlockDecl_add x y = case (x, y) of
  (BDReg nsr d, BDReg sr l) | nsr == sr -> add (BDReg sr) d l
  (BDInt d, BDInt l) -> add BDInt d l
  (BDReal d, BDReal l) -> add BDReal d l
  (BDTime d, BDTime l) -> add BDTime d l
  (BDRealTime d, BDRealTime l) -> add BDRealTime d l
  (BDEvent d, BDEvent l) -> add BDEvent d l
  (BDLocalParam nt d, BDLocalParam t l) | nt == t -> add (BDLocalParam t) d l
  _ -> Nothing
  where add f x l = Just $ f $ Compose $ runIdentity (getCompose x) <| getCompose l

-- | Converts ModGenSingleItem like `BlockDecl` into StdBlockDecl `BlockDecl`
toStdBlockDecl :: BD NonEmpty t -> NonEmpty (Identified (BlockDecl Identity t CExpr))
toStdBlockDecl x = case x of
  BDReg sr d -> conv (BDReg sr) d
  BDInt d -> conv BDInt d
  BDReal d -> conv BDReal d
  BDTime d -> conv BDTime d
  BDRealTime d -> conv BDRealTime d
  BDEvent d -> conv BDEvent d
  BDLocalParam t d -> conv (BDLocalParam t) d
  where conv f = fmap (fmap $ f . Identity) . getCompose

-- | Converts `SpecifySingleItem` into `SpecifyBlockedItem`s
toSpecBlockedItem ::
  SpecifySingleItem NExpr CExpr MPExpr -> NonEmpty (SpecifyBlockedItem NExpr CExpr MPExpr)
toSpecBlockedItem x = case x of
  SISpecParam rng d -> conv (SISpecParam rng) d
  SIPulsestyleOnevent st -> conv SIPulsestyleOnevent st
  SIPulsestyleOndetect st -> conv SIPulsestyleOndetect st
  SIShowcancelled st -> conv SIShowcancelled st
  SINoshowcancelled st -> conv SINoshowcancelled st
  SIPathDeclaration mpc con pol eds v -> [SIPathDeclaration mpc con pol eds v]
  SISetup a -> [SISetup a]
  SIHold a -> [SIHold a]
  SISetupHold a aa -> [SISetupHold a aa]
  SIRecovery a -> [SIRecovery a]
  SIRemoval a -> [SIRemoval a]
  SIRecrem a aa -> [SIRecrem a aa]
  SISkew a -> [SISkew a]
  SITimeSkew a ev rem -> [SITimeSkew a ev rem]
  SIFullSkew a tcl ev rem -> [SIFullSkew a tcl ev rem]
  SIPeriod ref tcl s -> [SIPeriod ref tcl s]
  SIWidth ref tcl t s -> [SIWidth ref tcl t s]
  SINoChange ref dat st en s -> [SINoChange ref dat st en s]
  where conv f = fmap (f . Identity)

fromSpecBlockedItem1 ::
  SpecifyBlockedItem NExpr CExpr MPExpr -> SpecifySingleItem NExpr CExpr MPExpr
fromSpecBlockedItem1 x = case x of
  SISpecParam rng d -> conv (SISpecParam rng) d
  SIPulsestyleOnevent st -> conv SIPulsestyleOnevent st
  SIPulsestyleOndetect st -> conv SIPulsestyleOndetect st
  SIShowcancelled st -> conv SIShowcancelled st
  SINoshowcancelled st -> conv SINoshowcancelled st
  SIPathDeclaration mpc con pol eds v -> SIPathDeclaration mpc con pol eds v
  SISetup a -> SISetup a
  SIHold a -> SIHold a
  SISetupHold a aa -> SISetupHold a aa
  SIRecovery a -> SIRecovery a
  SIRemoval a -> SIRemoval a
  SIRecrem a aa -> SIRecrem a aa
  SISkew a -> SISkew a
  SITimeSkew a ev rem -> SITimeSkew a ev rem
  SIFullSkew a tcl ev rem -> SIFullSkew a tcl ev rem
  SIPeriod ref tcl s -> SIPeriod ref tcl s
  SIWidth ref tcl t s -> SIWidth ref tcl t s
  SINoChange ref dat st en s -> SINoChange ref dat st en s
  where conv f = f . (:|[]) . runIdentity

fromSpecBlockedItem_add ::
  SpecifyBlockedItem NExpr CExpr MPExpr ->
  SpecifySingleItem NExpr CExpr MPExpr ->
  Maybe (SpecifySingleItem NExpr CExpr MPExpr)
fromSpecBlockedItem_add x y = case (x, y) of
  (SISpecParam nrng d, SISpecParam rng l) | nrng == rng-> add (SISpecParam rng) d l
  (SIPulsestyleOnevent st, SIPulsestyleOnevent l) -> add SIPulsestyleOnevent st l
  (SIPulsestyleOndetect st, SIPulsestyleOndetect l) -> add SIPulsestyleOndetect st l
  (SIShowcancelled st, SIShowcancelled l) -> add SIShowcancelled st l
  (SINoshowcancelled st, SINoshowcancelled l) -> add SINoshowcancelled st l
  _ -> Nothing
  where add f x y = Just $ f $ runIdentity x <| y

-- | Converts `SpecifyBlockedItem`s into `SpecifySingleItem`s
fromSpecBlockedItem ::
  [SpecifyBlockedItem NExpr CExpr MPExpr] -> [SpecifySingleItem NExpr CExpr MPExpr]
fromSpecBlockedItem = nonEmpty [] $ toList . regroup fromSpecBlockedItem1 fromSpecBlockedItem_add

-- | Converts `Gate` types
toBlockedGate :: Gate NonEmpty NExpr CExpr -> NonEmpty (Gate Identity NExpr CExpr)
toBlockedGate x = case x of
  GCMos r d3 l -> conv (GCMos r d3) l
  GEnable r b ds d3 l -> conv (GEnable r b ds d3) l
  GMos r np d3 l -> conv (GMos r np d3) l
  GNIn nt n ds d2 l -> conv (GNIn nt n ds d2) l
  GNOut r ds d2 l -> conv (GNOut r ds d2) l
  GPassEn r b d2 l -> conv (GPassEn r b d2) l
  GPass r l -> conv (GPass r) l
  GPull b ds l -> conv (GPull b ds) l
  where conv f = fmap (f . Identity)

fromBlockedGate1 :: Gate Identity NExpr CExpr -> Gate NonEmpty NExpr CExpr
fromBlockedGate1 x = case x of
  GCMos r d3 i -> conv (GCMos r d3) i
  GEnable r b ds d3 i -> conv (GEnable r b ds d3) i
  GMos r np d3 i -> conv (GMos r np d3) i
  GNIn nt n ds d2 i -> conv (GNIn nt n ds d2) i
  GNOut r ds d2 i -> conv (GNOut r ds d2) i
  GPassEn r b d2 i -> conv (GPassEn r b d2) i
  GPass r i -> conv (GPass r) i
  GPull b ds i -> conv (GPull b ds) i
  where conv f = f . (:|[]) . runIdentity

fromBlockedGate_add ::
  Gate Identity NExpr CExpr -> Gate NonEmpty NExpr CExpr -> Maybe (Gate NonEmpty NExpr CExpr)
fromBlockedGate_add x y = case (x, y) of
  (GCMos nr nd3 i, GCMos r d3 l) | nr == r && nd3 == d3 -> add (GCMos r d3) i l
  (GEnable nr nb nds nd3 i, GEnable r b ds d3 l)
   | nr == r && nb == b && nds == ds && nd3 == d3 -> add (GEnable r b ds d3) i l
  (GMos nr nnp nd3 i, GMos r np d3 l) | nr == r && nnp == np && nd3 == d3 -> add (GMos r np d3) i l
  (GNIn nnt nn nds nd2 i, GNIn nt n ds d2 l)
    | nnt == nt && nn == n && nds == ds && nd2 == d2 -> add (GNIn nt n ds d2) i l
  (GNOut nr nds nd2 i, GNOut r ds d2 l) | nr == r && nds == ds && nd2 == d2 ->
    add (GNOut r ds d2) i l
  (GPassEn nr nb nd2 i, GPassEn r b d2 l) | nr == r && nb == b && nd2 == d2 ->
    add (GPassEn r b d2) i l
  (GPass nr i, GPass r l) | nr == r -> add (GPass r) i l
  (GPull nb nds i, GPull b ds l) | nb == b && nds == ds -> add (GPull b ds) i l
  _ -> Nothing
  where add f x y = Just $ f $ runIdentity x <| y

-- | Converts `ModGenSingleItem` into `ModGenBlockedItem`s
toMGBlockedItem :: ModGenSingleItem NExpr CExpr -> NonEmpty (ModGenBlockedItem NExpr CExpr)
toMGBlockedItem x = case x of
  MGINetInit nt ds np ni -> conv (MGINetInit nt ds np) ni
  MGINetDecl nt np nd -> conv (MGINetDecl nt np) nd
  MGITriD ds np ni -> conv (MGITriD ds np) ni
  MGITriC cs np nd -> conv (MGITriC cs np) nd
  MGIBlockDecl d -> fmap MGIBlockDecl $ toMGIBlockDecl d
  MGIGenVar i -> conv MGIGenVar i
  MGITask b i d s -> [MGITask b i d s]
  MGIFunc b t i d s -> [MGIFunc b t i d s]
  MGIDefParam po -> conv MGIDefParam po
  MGIContAss ds d3 na -> conv (MGIContAss ds d3) na
  MGIGate g -> MGIGate <$> toBlockedGate g
  MGIUDPInst udp ds d2 i -> conv (MGIUDPInst udp ds d2) i
  MGIModInst mod pa i -> conv (MGIModInst mod pa) i
  MGIUnknownInst t p i -> conv (MGIUnknownInst t p) i
  MGIInitial s -> [MGIInitial s]
  MGIAlways s -> [MGIAlways s]
  MGILoopGen ii iv c ui uv b -> [MGILoopGen ii iv c ui uv b]
  MGICondItem ci -> [MGICondItem ci]
  where conv f = fmap (f . Identity)

fromMGBlockedItem1 :: ModGenBlockedItem NExpr CExpr -> ModGenSingleItem NExpr CExpr
fromMGBlockedItem1 x = case x of
  MGINetInit nt ds np ni -> conv (MGINetInit nt ds np) ni
  MGINetDecl nt np nd -> conv (MGINetDecl nt np) nd
  MGITriD ds np ni -> conv (MGITriD ds np) ni
  MGITriC cs np nd -> conv (MGITriC cs np) nd
  MGIBlockDecl d -> MGIBlockDecl $ fromMGIBlockDecl1 d
  MGIGenVar i -> conv MGIGenVar i
  MGITask b i d s -> MGITask b i d s
  MGIFunc b t i d s -> MGIFunc b t i d s
  MGIDefParam po -> conv MGIDefParam po
  MGIContAss ds d3 na -> conv (MGIContAss ds d3) na
  MGIGate g -> MGIGate $ fromBlockedGate1 g
  MGIUDPInst udp ds d2 i -> conv (MGIUDPInst udp ds d2) i
  MGIModInst mod pa i -> conv (MGIModInst mod pa) i
  MGIUnknownInst t p i -> conv (MGIUnknownInst t p) i
  MGIInitial s -> MGIInitial s
  MGIAlways s -> MGIAlways s
  MGILoopGen ii iv c ui uv b -> MGILoopGen ii iv c ui uv b
  MGICondItem ci -> MGICondItem ci
  where conv f = f . (:|[]) . runIdentity

fromMGBlockedItem_add ::
  ModGenBlockedItem NExpr CExpr ->
  ModGenSingleItem NExpr CExpr ->
  Maybe (ModGenSingleItem NExpr CExpr)
fromMGBlockedItem_add x y = case (x, y) of
  (MGINetInit nnt nds nnp ni, MGINetInit nt ds np l) | nnt == nt && nds == ds && nnp == np ->
    add (MGINetInit nt ds np) ni l
  (MGINetDecl nnt nnp nd, MGINetDecl nt np l) | nnt == nt && nnp == np ->
    add (MGINetDecl nt np) nd l
  (MGITriD nds nnp ni, MGITriD ds np l) | nds == ds && nnp == np -> add (MGITriD ds np) ni l
  (MGITriC ncs nnp nd, MGITriC cs np l) | ncs == cs && nnp == np -> add (MGITriC cs np) nd l
  (MGIBlockDecl d, MGIBlockDecl l) -> MGIBlockDecl <$> fromMGIBlockDecl_add d l
  (MGIGenVar i, MGIGenVar l) -> add MGIGenVar i l
  (MGIDefParam po, MGIDefParam l) -> add MGIDefParam po l
  (MGIContAss nds nd3 na, MGIContAss ds d3 l) | nds == ds && nd3 == d3 ->
    add (MGIContAss ds d3) na l
  (MGIGate g, MGIGate gl) -> MGIGate <$> fromBlockedGate_add g gl
  (MGIUDPInst nudp nds nd2 i, MGIUDPInst udp ds d2 l)
    | nudp == udp && nds == ds && nd2 == d2 -> add (MGIUDPInst udp ds d2) i l
  (MGIModInst nmod npa i, MGIModInst mod pa l) | nmod == mod && npa == pa ->
    add (MGIModInst mod pa) i l
  (MGIUnknownInst nt np i, MGIUnknownInst t p l) | nt == t && np == p ->
    add (MGIUnknownInst t p) i l
  _ -> Nothing
  where add f x y = Just $ f $ runIdentity x <| y

-- | Converts `ModGenBlockedItem`s into `ModGenSingleItem`s
fromMGBlockedItem ::
  [Attributed (ModGenBlockedItem NExpr CExpr)] -> [Attributed (ModGenSingleItem NExpr CExpr)]
fromMGBlockedItem =
  nonEmpty [] $ toList . regroup (fmap fromMGBlockedItem1) (addAttributed fromMGBlockedItem_add)

-- | Resolves Module and Primitive instantiation if possible
-- | Also checks there are no duplicate toplevel elements
resolveInsts :: Verilog2005 -> Either String Verilog2005
resolveInsts v = do
  nm <-
    foldr
      ( \m h -> h >>= let Identifier k = _mbIdent m
                       in HashMap.alterF (maybe (Right $ Just True) $ const $ Left $ duperr k) k
      )
      (Right HashMap.empty)
      (_vModule v)
  nm <-
    foldr
      (\p h -> h >>= let Identifier k = _pbIdent p
                      in HashMap.alterF (maybe (Right $ Just False) $ const $ Left $ duperr k) k
      )
      (Right nm)
      (_vPrimitive v)
  return $ v & biplate %~ \mgi -> case mgi of
    MGIUnknownInst k@(Identifier i) param (Identity (UknInst n lv args)) ->
      case HashMap.lookup i nm of
        Nothing -> mgi
        Just False ->
          MGIUDPInst k dsDefault (either (D21 . MTMSingle) (uncurry $ on D22 MTMSingle) <$> param) $
            Identity $ UDPInst (Just n) lv args
        Just True ->
          MGIModInst
            k
            ( ParamPositional $
                case param of Nothing -> []; Just (Right (e0, e1)) -> [e0, e1]; Just (Left e) -> [e]
            )
            ( Identity $
                ModInst n $
                  PortPositional $ map (Attributed [] . Just) $ netlv2expr lv : NE.toList args
            )
    _ -> mgi
  where duperr = printf "module or primitive %s defined more than once" . show

-- | Fit the number to a sign and size
fitSnSz :: Bool -> Natural -> Integer -> Integer
fitSnSz sn sz v =
  if sn
    then let b = bit $ fromEnum $ sz - 1 in (v .&. (b - 1)) - (v .&. b)
    else v .&. (bit (fromEnum sz) - 1)
