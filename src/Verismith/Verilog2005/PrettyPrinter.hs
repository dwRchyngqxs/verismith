-- Module      : Verismith.Verilog2005.PrettyPrinter
-- Description : Pretty printer for the Verilog 2005 AST.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module Verismith.Verilog2005.PrettyPrinter
  ( genSource,
    LocalCompDir (..),
    lcdDefault,
    lcdTimescale,
    lcdCell,
    lcdPull,
    lcdDefNetType,
    PrintingOpts (..)
  )
where

import Control.Lens.TH
import Data.Bifunctor (first)
import Data.Functor.Compose
import Data.Functor.Identity
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import Control.Applicative (liftA2)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)
import Data.String
import qualified Data.Vector.Unboxed as V
import Verismith.Utils hiding (comma)
import Verismith.Verilog2005.Lexer
import Verismith.Verilog2005.AST
import Verismith.Verilog2005.LibPretty
import Verismith.Verilog2005.Utils

-- | All locally applicable properties controlled by compiler directives
data LocalCompDir = LocalCompDir
  { _lcdTimescale :: !(Maybe (Int, Int)),
    _lcdCell :: !Bool,
    _lcdPull :: !(Maybe Bool),
    _lcdDefNetType :: !(Maybe NetType)
  }

$(makeLenses ''LocalCompDir)

lcdDefault :: LocalCompDir
lcdDefault = LocalCompDir Nothing False Nothing $ Just NTWire

-- | Pretty-printer options
data PrintingOpts = PrintingOpts
  { _poEscapedSpace :: !Bool,
    _poTableSpace :: !Bool,
    _poEdgeControlZ_X :: !Bool
  }

type Print = Reader PrintingOpts Doc

-- | Generates a string from a line length limit and a Verilog AST
genSource :: Maybe Word -> PrintingOpts -> Verilog2005 -> LB.ByteString
genSource mw opts ast = layout mw $ runReader (prettyVerilog2005 ast) opts

-- | Comma separated concatenation
(<.>) :: Doc -> Doc -> Doc
(<.>) a b = a <> comma <+> b

-- | [] {} ()
brk :: Doc -> Doc
brk = encl lbracket rbracket

brc :: Doc -> Doc
brc = encl lbrace rbrace

par :: Doc -> Doc
par = encl lparen rparen

gpar :: Doc -> Doc
gpar = group . par

-- | Print only if boolean is false
piff :: Doc -> Bool -> Doc
piff d c = if c then mempty else d

-- | Print only if boolean is true
pift :: Doc -> Bool -> Doc
pift d c = if c then d else mempty

-- | Print maybe
pm :: (x -> Print) -> Maybe x -> Print
pm = maybe $ pure mempty

-- | Print Foldable with stuff arround when not empty
pf :: Foldable f => (Doc -> Doc -> Doc) -> Doc -> Doc -> (a -> Print) -> f a -> Print
pf g l r f = nonEmpty (pure mempty) (fmap (\d -> l <> d <> r) . foldrMap1 f (liftA2 g . f)) . toList

-- | Print Foldable
pl :: Foldable f => (Doc -> Doc -> Doc) -> (a -> Print) -> f a -> Print
pl g f = foldrMap1' (pure mempty) f (liftA2 g . f) . toList

-- | Regroup and prettyprint
prettyregroup ::
  (Doc -> Doc -> Doc) ->
  (y -> Print) ->
  (x -> y) ->
  (x -> y -> Maybe y) ->
  NonEmpty x ->
  Print
prettyregroup g p mk add = foldrMap1 p (liftA2 g . p) . regroup mk add

-- | The core difficulty of this pretty printer:
-- | Identifier can be escaped, escaped identifiers require a space or newline after them
-- | but I want to avoid double [spaces/line breaks] and spaces at the end of a line
-- | And this problem creeps in every AST node with possible identifier printed at the end
-- | like expressions
-- | So a prettyprinter that takes an AST node of type `a` has this type
-- | The second element of the result pair is the type of space that follows the first element
-- | unless what is next is a space or a newline, in which case it can be ignored and replaced
type PrettyIdent a = a -> Reader PrintingOpts (Doc, Doc)

-- | The `pure/return` of prettyIdent
mkid :: PrettyIdent Doc
mkid = pure . flip (,) softline

-- | Evaluates a PrettyIdent and collapse the last character
padj :: PrettyIdent a -> a -> Print
padj p = fmap (uncurry (<>)) . p

-- | Applies a function to the stuff before the last character
fpadj :: (Doc -> Doc) -> PrettyIdent a -> a -> Print
fpadj f p x = uncurry (<>) . first f <$> p x

gpadj :: PrettyIdent a -> a -> Print
gpadj = fpadj group

ngpadj :: PrettyIdent a -> a -> Print
ngpadj = fpadj ng

-- | Just inserts a group before the last space of a PrettyIdent
-- | Useful to separate the break priority of the inside and outside space
mkg :: PrettyIdent x -> PrettyIdent x
mkg f = fmap (first group) . f

-- | In this version the group asks for raising the indentation level
mkng :: PrettyIdent x -> PrettyIdent x
mkng f = fmap (first ng) . f

-- | Print a comma separated list, haskell style: `a, b, c` or
-- | ```
-- | a
-- | , b
-- | , c
-- | ```
csl :: Foldable f => Doc -> Doc -> (a -> Print) -> f a -> Print
csl = pf $ \a b -> a </> comma <+> b

csl1 :: (a -> Print) -> NonEmpty a -> Print
csl1 f = foldrMap1 f $ liftA2 (\a b -> a </> comma <+> b) . f

cslid1 :: PrettyIdent a -> PrettyIdent (NonEmpty a)
cslid1 f = foldrMap1 f $ liftA2 (\x -> first (x <.>)) . padj f

cslid :: Foldable f => Doc -> Doc -> PrettyIdent a -> f a -> Print
cslid a b f = nonEmpty (pure mempty) (fmap (\x -> a <> x <> b) . padj (cslid1 f)) . toList

bcslid1 :: PrettyIdent a -> NonEmpty a -> Print
bcslid1 f l = brc . nest <$> padj (cslid1 $ mkg f) l

pcslid :: Foldable f => PrettyIdent a -> f a -> Print
pcslid f = cslid (lparen <> softspace) rparen $ mkg f

prettyBS :: PrettyIdent B.ByteString
prettyBS i = do
  bs <- asks _poEscapedSpace
  let si = isIdentSimple i
  return
    ( if si then raw i else "\\" <> if bs then raw i <> space else raw i,
      if si || bs then softline else newline
    )

prettyIdent :: PrettyIdent Identifier
prettyIdent (Identifier i) = prettyBS i

rawId :: Identifier -> Print
rawId (Identifier i) = do
  bs <- asks _poEscapedSpace
  let si = isIdentSimple i
  return $ if si then raw i else "\\" <> if bs then raw i <> space else raw i

-- | Somehow the next eight function are very common patterns
pspWith :: PrettyIdent a -> Doc -> PrettyIdent a
pspWith f d i = if nullDoc d then f i else (\x -> fst x <=> d) <$> f i >>= mkid

padjWith :: PrettyIdent a -> Doc -> PrettyIdent a
padjWith f d i = if nullDoc d then f i else (<> d) <$> padj f i >>= mkid

prettyEq :: Doc -> (Doc, Doc) -> (Doc, Doc)
prettyEq i = first $ \x -> ng $ group i <=> equals <+> group x

prettyAttrng :: Attributes -> Doc -> Print
prettyAttrng a x = (<?=> ng x) <$> prettyAttr a

prettyItem :: Attributes -> Doc -> Print
prettyItem a x = (<> semi) <$> prettyAttrng a x

prettyItems :: Doc -> (a -> Print) -> NonEmpty a -> Print
prettyItems h f b = (\x -> group h <=> group x <> semi) <$> csl1 (fmap ng . f) b

prettyItemsid :: Doc -> PrettyIdent a -> NonEmpty a -> Print
prettyItemsid h f b = (\x -> group h <=> x <> semi) <$> gpadj (cslid1 $ mkng f) b

prettyAttrThen :: Attributes -> Print -> Print
prettyAttrThen = liftA2 (<?=>) . prettyAttr

prettyAttr :: Attributes -> Print
prettyAttr = pl (<=>) $ nonEmpty (pure mempty) $ fmap (\x -> group $ "(* " <> fst x <=> "*)") . cslid1 pa
  where
    pa (Attribute i e) = maybe (prettyBS i) (liftA2 prettyEq (rawId $ Identifier i) . pca) e
    pca = prettyExpr prettyIdent (pm prettyCRangeExpr) (const $ pure mempty) 12

prettyHierIdent :: PrettyIdent (HierIdent CExpr)
prettyHierIdent (HierIdent p i) = do
  (ii, s) <- prettyIdent i
  iii <- foldrM (\x acc -> (\d -> d <> dot <> acc) <$> phId x) ii p
  return (nest iii, s)
  where
    phId (s, r) =
      pm (fmap (group . brk) . padj prettyCExpr) r >>= \dr -> ngpadj (padjWith prettyIdent dr) s

prettyDot1Ident :: PrettyIdent Dot1Ident
prettyDot1Ident (Dot1Ident mh t) = do
  (i, s) <- prettyIdent t
  case mh of Nothing -> return (i, s); Just h -> (\ft -> (ft <> dot <> i, s)) <$> padj prettyBS h

prettySpecTerm :: PrettyIdent (SpecTerm CExpr)
prettySpecTerm (SpecTerm i r) = pm prettyCRangeExpr r >>= \d -> padjWith prettyIdent d i

prettyNumber :: Number -> Doc
prettyNumber x = case x of
  NBinary l -> "b" </> fromString (concatMap show l)
  NOctal l -> "o" </> fromString (concatMap show l)
  NDecimal i -> "d" </> viaShow i
  NHex l -> "h" </> fromString (concatMap show l)
  NXZ b -> if b then "dx" else "dz"

prettyNumIdent :: PrettyIdent NumIdent
prettyNumIdent x = case x of
  NIIdent i -> prettyIdent i
  NIReal r -> mkid $ raw r
  NINumber n -> mkid $ viaShow n

prettyPrim :: PrettyIdent i -> (r -> Print) -> (a -> Print) -> PrettyIdent (Prim i r a)
prettyPrim ppid ppr ppa x = case x of
  PrimNumber 0 True (NDecimal i) -> mkid $ viaShow i
  PrimNumber w b n ->
    mkid $
      nest $
        (if w == 0 then mempty else viaShow w <> softline)
          <> group ((if b then "'s" else squote) <> prettyNumber n)
  PrimReal r -> mkid $ raw r
  PrimIdent i r -> ppr r >>= \rng -> first nest <$> padjWith ppid (group rng) i
  PrimConcat l -> bcslid1 pexpr l >>= mkid
  PrimMultConcat e l ->
    liftA2 (<>) (gpadj (prettyExpr prettyIdent (pm prettyCRangeExpr) ppa 12) e) (bcslid1 pexpr l)
      >>= mkid . brc . nest
  PrimFun i a l -> do
    dat <- ppa a
    darg <- pcslid pexpr l
    (if nullDoc dat then padjWith else pspWith) ppid (dat <?=> darg) i
  PrimSysFun i l -> pcslid pexpr l >>= mkid . \x -> nest $ "$" <> raw i <> x
  PrimMinTypMax m -> padj (prettyMTM pexpr) m >>= mkid . par
  PrimString x -> mkid $ "\"" <> raw x <> "\""
  where pexpr = prettyExpr ppid ppr ppa 12

preclevel :: BinaryOperator -> Int
preclevel b = case b of
  BinPower -> 1
  BinTimes -> 2
  BinDiv -> 2
  BinMod -> 2
  BinPlus -> 3
  BinMinus -> 3
  BinLSL -> 4
  BinLSR -> 4
  BinASL -> 4
  BinASR -> 4
  BinLT -> 5
  BinLEq -> 5
  BinGT -> 5
  BinGEq -> 5
  BinEq -> 6
  BinNEq -> 6
  BinCEq -> 6
  BinCNEq -> 6
  BinAnd -> 7
  BinXor -> 8
  BinXNor -> 8
  BinOr -> 9
  BinLAnd -> 10
  BinLOr -> 11

prettyExpr :: PrettyIdent i -> (r -> Print) -> (a -> Print) -> Int -> PrettyIdent (Expr i r a)
prettyExpr ppid ppr ppa l e = case e of
  ExprPrim e -> first group <$> prettyPrim ppid ppr ppa e
  ExprUnOp op a e -> do
    da <- ppa a
    (x, s) <- prettyPrim ppid ppr ppa e
    return (ng $ viaShow op <> piff (space <> da <> newline) (nullDoc da) <> x, s)
  ExprBinOp el op a r -> do
    let
      p = preclevel op
      pp = pexpr $ p - 1
    ll <- (<=> viaShow op) <$> psexpr p el
    da <- ppa a
    case compare l p of
      LT -> padj pp r >>= mkid . ng . par . (ll <+>) . (da <?=>)
      EQ -> first ((ll <+>) . (da <?=>)) <$> pp r
      GT -> first (ng . (ll <+>) . (da <?=>)) <$> pp r
  ExprCond ec a et ef -> do
    dc <- psexpr 11 ec
    dt <- psexpr 12 et
    df <- pexpr 12 ef
    da <- ppa a
    let
      pp = first (\x -> nest $ group (dc <=> nest ("?" <?+> da)) <=> group (dt <=> colon <+> x)) df
    if l < 12 then mkid $ gpar $ uncurry (<>) pp else return pp
  where
    pexpr = prettyExpr ppid ppr ppa
    psexpr n e = fst <$> pexpr n e

prettyNExpr :: PrettyIdent NExpr
prettyNExpr (NExpr e) = prettyExpr prettyHierIdent (pm prettyNDimRange) prettyAttr 12 e

prettyCExpr :: PrettyIdent CExpr
prettyCExpr (CExpr e) = prettyExpr prettyIdent (pm prettyCRangeExpr) prettyAttr 12 e

prettyMTM :: PrettyIdent et -> PrettyIdent (MinTypMax et)
prettyMTM pp x = case x of
  MTMSingle e -> pp e
  MTMFull l t h -> do
    mn <- gpadj pp l
    mt <- gpadj pp t
    first (\mx -> mn <> colon <-> mt <> colon <-> group mx) <$> pp h

prettyNMTM :: PrettyIdent (MinTypMax NExpr)
prettyNMTM = prettyMTM prettyNExpr

prettyCMTM :: PrettyIdent (MinTypMax CExpr)
prettyCMTM = prettyMTM prettyCExpr

prettyRange2 :: Range2 CExpr -> Print
prettyRange2 (Range2 m l) = do
  mx <- gpadj prettyCExpr m
  mn <- gpadj prettyCExpr l
  return $ brk $ mx <> colon <-> mn

prettyR2s :: [Range2 CExpr] -> Print
prettyR2s = pl (</>) $ fmap group . prettyRange2

prettyRangeExpr :: PrettyIdent e -> RangeExpr e CExpr -> Print
prettyRangeExpr pp x = case x of
  RESingle r -> brk <$> padj pp r
  REPair r2 -> prettyRange2 r2
  REBaseOff b mp o -> do
    base <- gpadj pp b
    off <- gpadj prettyCExpr o
    return $ brk $ base <> (if mp then "-" else "+") <> colon <-> off

prettyCRangeExpr :: RangeExpr CExpr CExpr -> Print
prettyCRangeExpr = prettyRangeExpr prettyCExpr

prettyDimRange :: PrettyIdent e -> DimRange e CExpr -> Print
prettyDimRange pp (DimRange d r) =
  foldr (liftA2 ((</>) . group . brk) . padj pp) (group <$> prettyRangeExpr pp r) d

prettyNDimRange :: DimRange NExpr CExpr -> Print
prettyNDimRange = prettyDimRange prettyNExpr

prettyCDimRange :: DimRange CExpr CExpr -> Print
prettyCDimRange = prettyDimRange prettyCExpr

prettySignRange :: SignRange CExpr -> Print
prettySignRange (SignRange s r) = (pift "signed" s <?=>) <$> pm (fmap group . prettyRange2) r

prettyComType :: (d -> Doc) -> ComType d CExpr -> Print
prettyComType f x = case x of
  CTAbstract t -> pure $ viaShow t
  CTConcrete e sr -> group . (f e <?=>) <$> prettySignRange sr

prettyDriveStrength :: DriveStrength -> Doc
prettyDriveStrength x = case x of
  DSNormal StrStrong StrStrong -> mempty
  DSNormal s0 s1 -> par $ viaShow s0 <> "0" </> comma <+> viaShow s1 <> "1"
  DSHighZ False s -> par $ "highz0" </> comma <+> viaShow s <> "1"
  DSHighZ True s -> par $ viaShow s <> "0" </> comma <+> "highz1"

prettyDelay3 :: PrettyIdent (Delay3 NExpr)
prettyDelay3 x =
  first ("#" <>) <$> case x of
    D3Base ni -> prettyNumIdent ni
    D31 m -> padj prettyNMTM m >>= mkid . par
    D32 m1 m2 -> do
      d1 <- ngpadj prettyNMTM m1
      d2 <- ngpadj prettyNMTM m2
      mkid $ par $ d1 <.> d2
    D33 m1 m2 m3 -> do
      d1 <- ngpadj prettyNMTM m1
      d2 <- ngpadj prettyNMTM m2
      d3 <- ngpadj prettyNMTM m3
      mkid $ par $ d1 <.> d2 <.> d3

prettyDelay2 :: PrettyIdent (Delay2 NExpr)
prettyDelay2 x =
  first ("#" <>) <$> case x of
    D2Base ni -> prettyNumIdent ni
    D21 m -> padj prettyNMTM m >>= mkid . par
    D22 m1 m2 -> do
      d1 <- ngpadj prettyNMTM m1
      d2 <- ngpadj prettyNMTM m2
      mkid $ par $ d1 <.> d2

prettyDelay1 :: PrettyIdent (Delay1 NExpr)
prettyDelay1 x =
  first ("#" <>) <$> case x of
    D1Base ni -> prettyNumIdent ni
    D11 m -> padj prettyNMTM m >>= mkid . par

prettyLValue :: PrettyIdent e -> PrettyIdent (LValue e CExpr)
prettyLValue f x = case x of
  LVSingle hi r -> do
    rng <- pm (fmap group . prettyDimRange f) r
    first nest <$> padjWith prettyHierIdent rng hi
  LVConcat l -> bcslid1 (prettyLValue f) l >>= mkid

prettyNetLV :: PrettyIdent (LValue CExpr CExpr)
prettyNetLV = prettyLValue prettyCExpr

prettyVarLV :: PrettyIdent (LValue NExpr CExpr)
prettyVarLV = prettyLValue prettyNExpr

prettyAssign :: PrettyIdent e -> PrettyIdent (Assign e CExpr NExpr)
prettyAssign f (Assign l e) = liftA2 (prettyEq . fst) (prettyLValue f l) (prettyNExpr e)

prettyNetAssign :: PrettyIdent (Assign CExpr CExpr NExpr)
prettyNetAssign = prettyAssign prettyCExpr

prettyVarAssign :: PrettyIdent (Assign NExpr CExpr NExpr)
prettyVarAssign = prettyAssign prettyNExpr

prettyEventControl :: PrettyIdent (EventControl NExpr CExpr)
prettyEventControl x =
  first ("@" <>) <$> case x of
    ECDeps -> pure ("*", newline)
    ECIdent hi -> first ng <$> prettyHierIdent hi
    ECExpr l -> (\e -> (gpar e, newline)) <$> padj (cslid1 pEP) l
  where
    pEP (EventPrim p e) =
      first (ng . ((case p of EPAny -> mempty; EPPos -> "posedge"; EPNeg -> "negedge") <?=>))
        <$> prettyNExpr e

prettyEdgeDesc :: EdgeDesc -> Print
prettyEdgeDesc x = do
  zx <- asks _poEdgeControlZ_X
  if x == V.fromList [True, True, False, False, False, True]
    then pure "posedge"
    else
      if x == V.fromList [False, False, True, True, True, False]
        then pure "negedge"
        else group . ("edge" <=>) . brk <$> csl mempty mempty (pure . raw) (V.ifoldr (pED zx) [] x)
  where
    pED zx i b =
      if b
        then (:) $ case i of
          0 -> "01"
          1 -> if zx then "0z" else "0x"
          2 -> "10"
          3 -> if zx then "1z" else "1x"
          4 -> if zx then "z0" else "x0"
          5 -> if zx then "z1" else "x1"
        else id

prettyXparam :: B.ByteString -> ComType () CExpr -> NonEmpty (Identified (MinTypMax CExpr)) -> Print
prettyXparam pre t l = do
  dt <- prettyComType (const mempty) t
  prettyItemsid
    (raw pre <?=> dt)
    (\(Identified i v) -> liftA2 prettyEq (rawId i) (prettyCMTM v))
    l

type EDI = Either [Range2 CExpr] CExpr

data AllBlockDecl
  = ABDReg (SignRange CExpr) (NonEmpty (Identified EDI))
  | ABDInt (NonEmpty (Identified EDI))
  | ABDReal (NonEmpty (Identified EDI))
  | ABDTime (NonEmpty (Identified EDI))
  | ABDRealTime (NonEmpty (Identified EDI))
  | ABDEvent (NonEmpty (Identified [Range2 CExpr]))
  | ABDLocalParam (ComType () CExpr) (NonEmpty (Identified (MinTypMax CExpr)))
  | ABDParameter (ComType () CExpr) (NonEmpty (Identified (MinTypMax CExpr)))
  | ABDPort Dir (ComType Bool CExpr) (NonEmpty Identifier)

fromBlockDecl ::
  (forall x. f x -> NonEmpty (Identified x)) -> (t -> EDI) -> BlockDecl f t CExpr -> AllBlockDecl
fromBlockDecl ff ft bd = case bd of
  BDReg sr x -> convt (ABDReg sr) x
  BDInt x -> convt ABDInt x
  BDReal x -> convt ABDReal x
  BDTime x -> convt ABDTime x
  BDRealTime x -> convt ABDRealTime x
  BDEvent r2 -> conv ABDEvent r2
  BDLocalParam t v -> conv (ABDLocalParam t) v
  where
    conv c = c . ff
    convt c = c . NE.map (\(Identified i x) -> Identified i $ ft x) . ff

fromStdBlockDecl :: AttrIded (StdBlockDecl CExpr) -> Attributed AllBlockDecl
fromStdBlockDecl (AttrIded a i sbd) =
  Attributed a $ case sbd of
    SBDParameter (Parameter t v) -> ABDParameter t [Identified i v]
    SBDBlockDecl bd -> fromBlockDecl ((:|[]) . Identified i . runIdentity) Left bd
    
prettyAllBlockDecl :: AllBlockDecl -> Print
prettyAllBlockDecl x = case x of
  ABDReg sr l -> prettySignRange sr >>= \dsr -> mkedi ("reg" <?=> dsr) l
  ABDInt l -> mkedi "integer" l
  ABDReal l -> mkedi "real" l
  ABDTime l -> mkedi "time" l
  ABDRealTime l -> mkedi "realtime" l
  ABDEvent l ->
    prettyItemsid "event" (\(Identified i r) -> prettyR2s r >>= \dr -> padjWith prettyIdent dr i) l
  ABDLocalParam t l -> prettyXparam "localparam" t l
  ABDParameter t l -> prettyXparam "parameter" t l
  ABDPort d t l ->
    prettyComType (pift "reg") t >>= \dt -> prettyItemsid (viaShow d <?=> dt) prettyIdent l
  where
    mkedi h =
      prettyItemsid h $
        \(Identified i edi) -> case edi of
          Left r2 -> prettyR2s r2 >>= \dim -> padjWith prettyIdent (group dim) i
          Right ce -> liftA2 prettyEq (rawId i) (prettyCExpr ce)

prettyAllBlockDecls :: [Attributed AllBlockDecl] -> Print
prettyAllBlockDecls =
  nonEmpty (pure mempty) $
    prettyregroup
      (<#>)
      (\(Attributed a abd) -> prettyAllBlockDecl abd >>= prettyAttrng a)
      id
      (addAttributed $ \x y -> case (x, y) of
        (ABDReg nsr nl, ABDReg sr l) | nsr == sr -> Just $ ABDReg sr $ nl <> l
        (ABDInt nl, ABDInt l) -> Just $ ABDInt $ nl <> l
        (ABDReal nl, ABDReal l) -> Just $ ABDReal $ nl <> l
        (ABDTime nl, ABDTime l) -> Just $ ABDTime $ nl <> l
        (ABDRealTime nl, ABDRealTime l) -> Just $ ABDRealTime $ nl <> l
        (ABDEvent nl, ABDEvent l) -> Just $ ABDEvent $ nl <> l
        (ABDLocalParam nt nl, ABDLocalParam t l) | nt == t -> Just $ ABDLocalParam t $ nl <> l
        (ABDParameter nt nl, ABDParameter t l) | nt == t -> Just $ ABDParameter t $ nl <> l
        (ABDPort nd nt nl, ABDPort d t l) | nd == d && nt == t -> Just $ ABDPort d t $ nl <> l
        _ -> Nothing
      )

prettyStdBlockDecls :: [AttrIded (StdBlockDecl CExpr)] -> Print
prettyStdBlockDecls = prettyAllBlockDecls . map fromStdBlockDecl

prettyTFBlockDecls :: (d -> Dir) -> [AttrIded (TFBlockDecl d CExpr)] -> Print
prettyTFBlockDecls f =
  prettyAllBlockDecls
    . map
      ( \(AttrIded a i x) -> case x of
          TFBDPort d t -> Attributed a $ ABDPort (f d) t [i]
          TFBDStd sbd -> fromStdBlockDecl (AttrIded a i sbd)
      )

prettyStatement :: Bool -> Statement NExpr CExpr -> Print
prettyStatement protect x = case x of
  SBlockAssign b (Assign lv v) dec -> do
    delev <- case dec of
      Nothing -> pure mempty
      Just (DECRepeat e ev) -> do
        ex <- padj prettyNExpr e
        evc <- prettyEventControl ev
        return $ group ("repeat" <=> gpar ex) <=> fst evc
      Just (DECDelay d) -> fst <$> prettyDelay1 d
      Just (DECEvent e) -> fst <$> prettyEventControl e
    ll <- prettyVarLV lv
    rr <- gpadj prettyNExpr v
    return $ ng $ group (fst ll) <=> group (piff langle b <> equals <+> delev <?=> rr) <> semi
  SCase zox e b s -> do
    ex <- padj prettyNExpr e
    dft <- case s of
      Attributed [] Nothing -> pure mempty
      _ -> nest . ("default:" <=>) <$> prettyMybStmt False s
    let
      pci (CaseItem p v) = do
        pat <- gpadj (cslid1 prettyNExpr) p
        branch <- prettyMybStmt False v
        return $ pat <> colon <+> ng branch
    body <- pl (<#>) pci b
    return $
      block
        (nest $ "case" <> case zox of {ZOXZ -> "z"; ZOXO -> mempty; ZOXX -> "x"} <=> gpar ex)
        "endcase"
        (body <?#> dft)
  SIf c t f -> do
    head <- ("if" <=>) . gpar <$> padj prettyNExpr c
    case f of
      Attributed [] Nothing | protect == False -> (ng head <>) <$> prettyRMybStmt False t
      Attributed [] (Just x@(SBlock _ _ _)) -> do
        -- `else` and `begin`/`fork` at same indentation level
        tb <- prettyRMybStmt True t
        fb <- prettyStatement False x
        return $ ng (group head <> tb) <#> "else" <=> fb
      Attributed [] (Just x@(SIf _ _ _)) -> do
        -- `if` and `else if` at same indentation level
        tb <- prettyRMybStmt True t
        fb <- prettyStatement protect x
        return $ ng (group head <> tb) <#> "else" <=> fb
      _ -> do
        tb <- prettyRMybStmt True t
        fb <- prettyRMybStmt protect f
        return $ ng (group head <> tb) <#> nest ("else" <> fb)
  SDisable hi -> (\x -> group $ "disable" <=> x <> semi) <$> padj prettyHierIdent hi
  SEventTrigger hi e -> do
    dhi <- padj prettyHierIdent hi
    dim <- pl (</>) (fmap (group . brk) . padj prettyNExpr) e
    return $ group $ "->" <+> dhi <> dim <> semi
  SLoop ls s -> do
    head <- case ls of
      LSForever -> pure "forever"
      LSRepeat e -> ("repeat" <=>) . gpar <$> padj prettyNExpr e
      LSWhile e -> ("while" <=>) . gpar <$> padj prettyNExpr e
      LSFor i c u -> do
        di <- gpadj prettyVarAssign i
        dc <- gpadj prettyNExpr c 
        du <- gpadj prettyVarAssign u
        return $ "for" <=> gpar (di <> semi <+> dc <> semi <+> du)
    nest . (ng head <=>) <$> prettyAttrStmt protect s
  SProcContAssign pca ->
    (<> semi) . group <$> case pca of
      PCAAssign va -> ("assign" <=>) <$> padj prettyVarAssign va
      PCADeassign lv -> ("deassign" <=>) <$> padj prettyVarLV lv
      PCAForce lv -> ("force" <=>) <$> either (padj prettyVarAssign) (padj prettyNetAssign) lv
      PCARelease lv -> ("release" <=>) <$> either (padj prettyVarLV) (padj prettyNetLV) lv
  SProcTimingControl dec s -> do
    ddec <- either prettyDelay1 prettyEventControl dec
    (group (fst ddec) <=>) <$> prettyMybStmt protect s
  SBlock h ps s -> do
    head <- pm (\(s, _) -> (colon <+>) <$> rawId s) h
    block
      (nest $ (if ps then "fork" else "begin") <?/> head)
      (if ps then "join" else "end")
      <$> liftA2 (<?#>) (pm (prettyStdBlockDecls . snd) h) (pl (<#>) (prettyAttrStmt False) s)
  SSysTaskEnable s a ->
    (\x -> ng ("$" <> raw s <> x) <> semi) <$> pcslid (maybe (mkid mempty) prettyNExpr) a
  STaskEnable hi a -> do
    dhi <- padj prettyHierIdent hi
    args <- pcslid prettyNExpr a
    return $ group (dhi <> args) <> semi
  SWait e s -> padj prettyNExpr e >>= \x -> (ng ("wait" <=> gpar x) <>) <$> prettyRMybStmt protect s

prettyAttrStmt :: Bool -> Attributed (Statement NExpr CExpr) -> Print
prettyAttrStmt protect (Attributed a s) = prettyAttrThen a $ nest <$> prettyStatement protect s

prettyMybStmt :: Bool -> Attributed (Maybe (Statement NExpr CExpr)) -> Print
prettyMybStmt protect (Attributed a s) = case s of
  Nothing -> (<> semi) <$> prettyAttr a
  Just s -> prettyAttrThen a $ prettyStatement protect s

prettyRMybStmt :: Bool -> Attributed (Maybe (Statement NExpr CExpr)) -> Print
prettyRMybStmt protect (Attributed a s) = do
  da <- case a of {[] -> pure mempty; _ -> (newline <>) <$> prettyAttr a}
  ds <- maybe (pure semi) (fmap (newline <>) . prettyStatement protect) s
  return $ da <> ds

prettyPortAssign :: PortAssign NExpr -> Print
prettyPortAssign x = case x of
  PortPositional l ->
    cslid
      mempty
      mempty
      ( \(Attributed a e) -> case e of
          Nothing -> prettyAttr a >>= mkid
          Just e -> do
            (i, s) <- prettyNExpr e
            d <- prettyAttrng a i
            return (group d, s)
      )
      l
  PortNamed l ->
    csl
      mempty
      softline
      ( \(AttrIded a i e) -> do
          s <- padj prettyIdent i
          ex <- pm (padj prettyNExpr) e
          group <$> prettyAttrng a (dot <> s <> gpar ex)
      )
      l

prettyGate :: Gate NonEmpty NExpr CExpr -> Print
prettyGate x = case x of
  GCMos r d3 l -> do
    d <- pm (fmap fst . prettyDelay3) d3
    prettyItems
      (pift "r" r <> "cmos" <?=> d)
      ( \(GICMos n lv inp nc pc) -> do
          i <- pm pname n
          dlv <- ngpadj prettyNetLV lv
          di <- ngpadj prettyNExpr inp
          dn <- ngpadj prettyNExpr nc
          dp <- ngpadj prettyNExpr pc
          return $ i <> gpar (dlv <.> di <.> dn <.> dp)
      )
      l
  GEnable r oz ds d3 l -> do
    d <- pm (fmap fst . prettyDelay3) d3
    prettyItems
      ( (if r then "not" else "buf") <> "if" <> (if oz then "1" else "0")
          <?=> prettyDriveStrength ds
          <?=> d
      )
      ( \(GIEnable n lv inp e) -> do
          i <- pm pname n
          dlv <- ngpadj prettyNetLV lv
          di <- ngpadj prettyNExpr inp
          de <- ngpadj prettyNExpr e
          return $ i <> gpar (dlv <.> di <.> de)
      )
      l
  GMos r np d3 l -> do
    d <- pm (fmap fst . prettyDelay3) d3
    prettyItems
      (pift "r" r <> (if np then "n" else "p") <> "mos" <?=> d)
      ( \(GIMos n lv inp e) -> do
          i <- pm pname n
          dlv <- ngpadj prettyNetLV lv
          di <- ngpadj prettyNExpr inp
          de <- ngpadj prettyNExpr e
          return $ i <> gpar (dlv <.> di <.> de)
      )
      l
  GNIn nin n ds d2 l -> do
    d <- pm (fmap fst . prettyDelay2) d2
    prettyItems
      ( (if nin == NITXor then "x" <> pift "n" n <> "or" else pift "n" n <> viaShow nin)
          <?=> prettyDriveStrength ds
          <?=> d
      )
      ( \(GINIn n lv inp) -> do
          i <- pm pname n
          dlv <- ngpadj prettyNetLV lv
          di <- padj (cslid1 $ mkng prettyNExpr) inp
          return $ i <> gpar (dlv <.> di)
      )
      l
  GNOut r ds d2 l -> do
    d <- pm (fmap fst . prettyDelay2) d2
    prettyItems
      ((if r then "not" else "buf") <?=> prettyDriveStrength ds <?=> d)
      ( \(GINOut n lv inp) -> do
          i <- pm pname n
          dlv <- padj (cslid1 $ mkng prettyNetLV) lv
          di <- ngpadj prettyNExpr inp
          return $ i <> gpar (dlv <.> di)
      )
      l
  GPassEn r oz d2 l -> do
    d <- pm (fmap fst . prettyDelay2) d2
    prettyItems
      (pift "r" r <> "tranif" <> (if oz then "1" else "0") <?=> d)
      ( \(GIPassEn n ll rl e) -> do
          i <- pm pname n
          dll <- ngpadj prettyNetLV ll
          drl <- ngpadj prettyNetLV rl
          de <- ngpadj prettyNExpr e
          return $ i <> gpar (dll <.> drl <.> de)
      )
      l
  GPass r l ->
    prettyItems
      (pift "r" r <> "tran")
      ( \(GIPass n ll rl) -> do
          i <- pm pname n
          dll <- ngpadj prettyNetLV ll
          drl <- ngpadj prettyNetLV rl
          return $ i <> gpar (dll <.> drl))
      l
  GPull ud ds l ->
    prettyItems
      ("pull" <> (if ud then "up" else "down") <?=> prettyDriveStrength ds)
      (\(GIPull n lv) -> pm pname n >>= \i -> (i <>) . gpar <$> gpadj prettyNetLV lv)
      l
  where
    pname (InstanceName i r) = pm prettyRange2 r >>= \rng -> gpadj (padjWith prettyIdent rng) i

prettyModGenSingleItem :: ModGenSingleItem NExpr CExpr -> Bool -> Print
prettyModGenSingleItem x protect = case x of
  MGINetInit nt ds np l ->
    com np >>= \d -> prettyItemsid (viaShow nt <?=> prettyDriveStrength ds <?=> d) pide l
  MGINetDecl nt np l -> com np >>= \d -> prettyItemsid (viaShow nt <?=> d) pidd l
  MGITriD ds np l ->
    com np >>= \d -> prettyItemsid ("trireg" <?=> prettyDriveStrength ds <?=> d) pide l
  MGITriC cs np l ->
    com np >>= \d -> prettyItemsid ("trireg" <?=> piff (viaShow cs) (cs == CSMedium) <?=> d) pidd l
  MGIBlockDecl bd -> prettyAllBlockDecl $ fromBlockDecl getCompose id bd
  MGIGenVar l -> prettyItemsid "genvar" prettyIdent l
  MGITask aut s d b -> do
    i <- padj prettyIdent s
    decls <- prettyTFBlockDecls id d
    body <- prettyMybStmt False b
    return $
      block (ng $ group ("task" <?=> mauto aut) <=> i <> semi) "endtask" (decls <?#> nest body)
  MGIFunc aut t s d b -> do
    dt <- pm (prettyComType $ const mempty) t
    i <- padj prettyIdent s
    decls <- prettyTFBlockDecls (const DirIn) d
    body <- prettyStatement False $ toStatement b
    return $
      block
        (ng $ group ("function" <?=> mauto aut <?=> dt) <=> i <> semi)
        "endfunction"
        (decls <?#> nest body)
  MGIDefParam l ->
    prettyItemsid
      "defparam"
      (\(ParamOver hi v) -> liftA2 (prettyEq . fst) (prettyHierIdent hi) (prettyCMTM v))
      l
  MGIContAss ds d3 l -> do
    d <- pm (fmap fst . prettyDelay3) d3
    prettyItemsid ("assign" <?=> prettyDriveStrength ds <?=> d) prettyNetAssign l
  MGIGate g -> prettyGate g
  MGIUDPInst kind ds d2 l -> do
    d <- pm (fmap fst . prettyDelay2) d2
    dk <- rawId kind
    prettyItems
      (dk <?=> prettyDriveStrength ds <?=> d)
      ( \(UDPInst n lv args) -> do
          i <- pm pname n
          dlv <- gpadj prettyNetLV lv
          da <- padj (cslid1 $ mkg prettyNExpr) args
          return $ i <> gpar (dlv <.> da)
      )
      l
  MGIModInst kind param l -> do
    dp <- case param of
      ParamPositional l -> cslid ("#(" <> softspace) rparen (mkng prettyNExpr) l
      ParamNamed l ->
        csl
          ("#(" <> softspace)
          (softline <> rparen)
          ( \(Identified i e) -> do
              s <- padj prettyIdent i
              d <- pm (padj prettyNMTM) e
              return $ ng $ dot <> s <> gpar d
          )
          l
    dk <- rawId kind
    prettyItems
      (dk <?=> dp)
      (\(ModInst n args) -> pname n >>= \i -> (i <>) . gpar <$> prettyPortAssign args)
      l
  MGIUnknownInst kind param l -> do
    dp <- case param of
      Nothing -> pure mempty
      Just (Left e) -> ("#" <>) . par <$> padj prettyNExpr e
      Just (Right (e0, e1)) ->
        ("#" <>) . par <$> liftA2 (<.>) (gpadj prettyNExpr e0) (gpadj prettyNExpr e1)
    dk <- rawId kind
    prettyItems
      (dk <?=> dp)
      ( \(UknInst n lv args) -> do
          i <- pname n
          dlv <- gpadj prettyNetLV lv
          da <- padj (cslid1 $ mkg prettyNExpr) args
          return $ i <> gpar (dlv <.> da)
      )
      l
  MGIInitial s -> ("initial" <=>) <$> prettyAttrStmt protect s
  MGIAlways s -> ("always" <=>) <$> prettyAttrStmt protect s
  MGILoopGen si vi cond su vu (GenerateBlock i b) -> do
    di <- gpadj (liftA2 prettyEq (rawId si) . prettyCExpr) vi
    dc <- ngpadj prettyCExpr cond
    du <- gpadj (liftA2 prettyEq (rawId su) . prettyCExpr) vu
    let head = "for" <=> gpar (di <> semi <+> dc <> semi <+> du)
    case fromMGBlockedItem b of
      [Attributed a x] | i == Nothing ->
        ng . (group head <=>) <$> prettyAttrThen a (prettyModGenSingleItem x protect)
      r -> (ng head <=>) <$> prettyGBlock i r
  MGICondItem ci -> prettyModGenCondItem ci protect -- protect should never be True in practice
  where
    pname (InstanceName i r) = pm prettyRange2 r >>= \rng -> gpadj (padjWith prettyIdent rng) i
    mauto b = pift "automatic" b
    pidd (NetDecl i d) = prettyR2s d >>= \dim -> padjWith prettyIdent dim i
    pide (NetInit i e) = liftA2 prettyEq (rawId i) (prettyNExpr e)
    com (NetProp b vs d3) = do
      let s = pift "signed" b
      dvs <- maybe
        (pure s)
        ( \(vs, r2) ->
            (\x -> maybe mempty (\b -> if b then "vectored" else "scalared") vs <?=> s <?=> x)
              <$> prettyRange2 r2
        )
        vs
      (dvs <?=>) <$> pm (fmap fst . prettyDelay3) d3

-- | Nested conditionals with dangling else support
prettyModGenCondItem :: ModGenCondItem NExpr CExpr -> Bool -> Print
prettyModGenCondItem ci protect = case ci of
  MGCIIf c t f -> do
    head <- ("if" <=>) . gpar <$> padj prettyCExpr c
    case f of
      GCBEmpty -> (<?#> pift "else;" protect) <$> pGCB head protect t
      _ -> liftA2 (<#>) (pGCB head True t) (pGCB "else" protect f)
  MGCICase c b md -> do
    dc <- padj prettyCExpr c
    body <-
      pl
        (<#>)
        (\(GenCaseItem p v) -> gpadj (cslid1 prettyCExpr) p >>= \pat -> pGCB (pat <> colon) False v)
        b
    dd <- case md of GCBEmpty -> pure mempty; _ -> pGCB "default:" False md
    return $ block (ng $ "case" <=> gpar dc) "endcase" (body <?#> dd)
  where
    isNotCond x = case x of MGICondItem _ -> False; _ -> True
    pGCB head p b = case b of
      GCBEmpty -> pure $ head <> semi
      GCBConditional (Attributed a ci) ->
        ng . (group head <=>) <$> prettyAttrThen a (prettyModGenCondItem ci p)
      GCBBlock (GenerateBlock i b) -> case fromMGBlockedItem b of
        [Attributed a x] | i == Nothing && isNotCond x ->
          ng . (group head <=>) <$> prettyAttrThen a (prettyModGenSingleItem x protect)
        r -> (ng head <=>) <$> prettyGBlock i r

-- | Generate block
prettyGBlock :: Maybe Identifier -> [Attributed (ModGenSingleItem NExpr CExpr)] -> Print
prettyGBlock i l = do
  bn <- pm (fmap (colon <=>) . rawId) i
  block ("begin" <> bn) "end"
    <$> pl
      (<#>)
      (\(Attributed a x) -> prettyAttrThen a $ prettyModGenSingleItem x False)
      l

prettyGenerateBlock :: GenerateBlock NExpr CExpr -> Print
prettyGenerateBlock (GenerateBlock s x) = prettyGBlock s $ fromMGBlockedItem x

prettySpecParams :: Maybe (Range2 CExpr) -> NonEmpty (SpecParamDecl CExpr) -> Print
prettySpecParams rng l = do
  dr <- pm prettyRange2 rng
  prettyItemsid
    ("specparam" <?=> dr)
    ( \d -> case d of
        SPDAssign i v -> liftA2 prettyEq (rawId i) (prettyCMTM v)
        SPDPathPulse io r e -> do
          dpp <- pm pPP io
          dr <- ngpadj prettyCMTM r
          de <- if r == e then pure mempty else (comma <+>) <$> ngpadj prettyCMTM e
          prettyEq ("PATHPULSE$" <> dpp) <$> mkid (par $ dr <> de)
    )
    l
  where
    -- Change to a spaced layout if tools allow it
    pPP (i, o) = do
      din <- ngpadj prettySpecTerm i
      dout <- prettySpecTerm o
      return $ din <> "$" <> fst dout

prettyPathDecl :: SpecPath CExpr -> Maybe Bool -> Maybe (NExpr, Maybe Bool) -> Print
prettyPathDecl p pol eds = do
  -- parallel or full, source(s), destination(s)
  (pf, (cin, _), cout) <- case p of
    SPParallel i o -> do
      sti <- prettySpecTerm i
      sto <- prettySpecTerm o
      return (True, sti, fne sto)
    SPFull i o -> do
      sti <- ppSTs i
      sto <- ppSTs o
      return (False, sti, fne sto)
  (de, ed) <- case eds of
    Nothing -> pure (cout, mempty)
    Just (dst, me) -> do
      d <- padj prettyNExpr dst
      return
        ( par $ cout <> po <> colon <+> d,
          maybe mempty (\e -> if e then "posedge" else "negedge") me
        )
  return $ gpar $ ng (ed <?=> group cin) <=> pift po noedge <> (if pf then "=>" else "*>") <+> ng de
  where
    ppSTs = cslid1 $ mkng prettySpecTerm
    -- polarity
    po = maybe mempty (\p -> if p then "+" else "-") pol
    -- edge sensitive path polarity isn't at the same place as non edge sensitive
    noedge = eds == Nothing
    fne = if noedge then uncurry (<>) else (<> newline) . fst

prettySpecifyItem :: SpecifySingleItem NExpr CExpr MPExpr -> Print
prettySpecifyItem x =
  nest <$> case x of
    SISpecParam r l -> prettySpecParams r l
    SIPulsestyleOnevent o -> prettyItemsid "pulsestyle_onevent" prettySpecTerm o
    SIPulsestyleOndetect o -> prettyItemsid "pulsestyle_ondetect" prettySpecTerm o
    SIShowcancelled o -> prettyItemsid "showcancelled" prettySpecTerm o
    SINoshowcancelled o -> prettyItemsid "noshowcancelled" prettySpecTerm o
    SIPathDeclaration mpc p pol eds l -> do
      dmpc <- case mpc of
        MPCCond e ->
          group . ("if" <=>) . gpar
            <$> padj (prettyExpr prettyIdent (const $ pure mempty) prettyAttr 12) e
        MPCAlways -> pure mempty
        MPCNone -> pure "ifnone"
      dpd <- prettyPathDecl p pol eds
      d <- padj (fmap (prettyEq dpd) . cslid1 prettyCMTM . cPDV) l
      return $ dmpc <?=> d <> semi
    SISetup (STCArgs d r e n) -> (\x -> "$setup" </> x <> semi) <$> ppA (STCArgs r d e n)
    SIHold a -> (\x -> "$hold" </> x <> semi) <$> ppA a
    SISetupHold a aa -> (\x -> "$setuphold" </> x <> semi) <$> ppAA a aa
    SIRecovery a -> (\x -> "$recovery" </> x <> semi) <$> ppA a
    SIRemoval a -> (\x -> "$removal" </> x <> semi) <$> ppA a
    SIRecrem a aa -> (\x -> "$recrem" </> x <> semi) <$> ppAA a aa
    SISkew a -> (\x -> "$skew" </> x <> semi) <$> ppA a
    SITimeSkew (STCArgs de re tcl n) meb mra -> do
      dre <- pTCE re
      dde <- pTCE de
      dtcl <- pexpr tcl
      dn <- pid n
      dmeb <- pm (gpadj prettyCExpr) meb
      dmra <- pm (gpadj prettyCExpr) mra
      return $ "$timeskew" </> gpar (dre <.> dde <.> toc [dtcl, dn, dmeb, dmra]) <> semi
    SIFullSkew (STCArgs de re tcl0 n) tcl1 meb mra -> do
      dre <- pTCE re
      dde <- pTCE de
      dtcl0 <- pexpr tcl0
      dtcl1 <- pexpr tcl1
      dn <- pid n
      dmeb <- pm (gpadj prettyCExpr) meb
      dmra <- pm (gpadj prettyCExpr) mra
      return $ "$fullskew" </> gpar (dre <.> dde <.> dtcl0 <.> toc [dtcl1, dn, dmeb, dmra]) <> semi
    SIPeriod re tcl n -> do
      dre <- pCTCE re
      dtcl <- pexpr tcl
      dn <- prid n
      return $ "$period" </> par (dre <.> dtcl <> dn) <> semi
    SIWidth re tcl mt n -> do
      dre <- pCTCE re
      dtcl <- pexpr tcl
      dmt <- pm (gpadj prettyCExpr) mt
      dn <- pid n
      return $ "$width" </> gpar (dre <.> toc [dtcl, dmt, dn]) <> semi
    SINoChange re de so eo n -> do
      dre <- pTCE re
      dde <- pTCE de
      dso <- ngpadj prettyNMTM so
      deo <- ngpadj prettyNMTM eo
      dn <- prid n
      return $ "$nochange" </> gpar (dre <.> dde <.> dso <.> deo <> dn) <> semi
  where
    toc :: [Doc] -> Doc
    toc = trailoptcat (<.>)
    prid = pm $ fmap (comma <+>) . padj prettyIdent
    pid = pm $ padj prettyIdent
    pexpr = gpadj prettyNExpr
    pTCC st mbe = case mbe of
      Nothing -> gpadj prettySpecTerm st
      Just (b, e) -> do
        dst <- prettySpecTerm st
        de <- gpadj prettyNExpr e
        return $ group $ group (fst dst) <=> "&&&" <+> pift "~" b <?+> de
    pTCE (TimingCheckEvent ev st tc) = ng <$> liftA2 (<?=>) (pm prettyEdgeDesc ev) (pTCC st tc)
    pCTCE (ControlledTimingCheckEvent ev st tc) =
      ng <$> liftA2 (<?=>) (prettyEdgeDesc ev) (pTCC st tc)
    ppA (STCArgs de re tcl n) = do
      dre <- pTCE re
      dde <- pTCE de
      dtcl <- pexpr tcl
      dn <- prid n
      return $ gpar $ dre <.> dde <.> dtcl <> dn
    pIMTM = pm $ \(Identified i mr) ->
      pm (fmap (group . brk) . padj prettyCMTM) mr >>= \d -> ngpadj (padjWith prettyIdent d) i
    ppAA (STCArgs de re tcl0 n) (STCAddArgs tcl1 msc mtc mdr mdd) = do
      dre <- pTCE re
      dde <- pTCE de
      dtcl0 <- pexpr tcl0
      dtcl1 <- pexpr tcl1
      dn <- pid n
      dmsc <- pm (ngpadj prettyNMTM) msc
      dmtc <- pm (ngpadj prettyNMTM) mtc
      dmdr <- pIMTM mdr
      dmdd <- pIMTM mdd
      return $ gpar $ dre <.> dde <.> dtcl0 <.> toc [dtcl1, dn, dmsc, dmtc, dmdr, dmdd]
    cPDV x = case x of
      PDV1 e -> e :|[]
      PDV2 e1 e2 -> [e1, e2]
      PDV3 e1 e2 e3 -> [e1, e2, e3]
      PDV6 e1 e2 e3 e4 e5 e6 -> [e1, e2, e3, e4, e5, e6]
      PDV12 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 ->
        [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12]

data ModuleItem'
  = MI'MGI (Attributed (ModGenSingleItem NExpr CExpr))
  | MI'Port Attributes Dir (SignRange CExpr) (NonEmpty Identifier)
  | MI'Parameter Attributes (ComType () CExpr) (NonEmpty (Identified (MinTypMax CExpr)))
  | MI'GenReg [Attributed (ModGenSingleItem NExpr CExpr)]
  | MI'SpecParam Attributes (Maybe (Range2 CExpr)) (NonEmpty (SpecParamDecl CExpr))
  | MI'SpecBlock [SpecifySingleItem NExpr CExpr MPExpr]

prettyModuleItems :: [ModuleItem NExpr CExpr MPExpr] -> Print
prettyModuleItems =
  nonEmpty (pure mempty) $
    prettyregroup
      (<#>)
      ( \x -> case x of
        MI'MGI (Attributed a i) -> prettyAttrThen a $ prettyModGenSingleItem i False
        MI'Port a d sr l -> do
          dsr <- prettySignRange sr
          it <- prettyItemsid (viaShow d <?=> dsr) prettyIdent l
          prettyAttrng a it
        MI'Parameter a t l -> prettyXparam "parameter" t l >>= prettyAttrng a
        MI'GenReg l ->
          block "generate" "endgenerate" <$>
            pl (<#>) (\(Attributed a i) -> prettyAttrThen a $ prettyModGenSingleItem i False) l
        MI'SpecParam a r l -> prettySpecParams r l >>= prettyAttrng a
        MI'SpecBlock l -> block "specify" "endspecify" <$> pl (<#>) prettySpecifyItem l
      )
      ( \x -> case x of
        MIMGI i -> MI'MGI $ fromMGBlockedItem1 <$> i
        MIPort (AttrIded a i (d, sr)) -> MI'Port a d sr [i]
        MIParameter (AttrIded a i (Parameter t v)) -> MI'Parameter a t [Identified i v]
        MIGenReg l -> MI'GenReg $ fromMGBlockedItem l
        MISpecParam a r spd -> MI'SpecParam a r [spd]
        MISpecBlock l -> MI'SpecBlock $ fromSpecBlockedItem l
      )
      ( \mi mi' -> case (mi, mi') of
        (MIMGI mgi, MI'MGI l) -> MI'MGI <$> (addAttributed fromMGBlockedItem_add) mgi l
        (MIPort (AttrIded na i (nd, nsr)), MI'Port a d sr l) | na == a && nd == d && nsr == sr ->
          Just $ MI'Port a d sr $ i <| l
        (MIParameter (AttrIded na i (Parameter nt v)), MI'Parameter a t l) | na == a && nt == t ->
          Just $ MI'Parameter a t $ Identified i v <| l
        (MISpecParam na nr spd, MI'SpecParam a r l) | na == a && nr == r ->
          Just $ MI'SpecParam a r $ spd <| l
        _ -> Nothing
      )

prettyPortInter :: [Identified [Identified (Maybe (RangeExpr CExpr CExpr))]] -> Print
prettyPortInter =
  cslid mempty mempty $
    \(Identified i@(Identifier ii) l) -> case l of
      [Identified i' x] | i == i' -> first group <$> prettySpecTerm (SpecTerm i x)
      _ ->
        if B.null ii
          then portexpr l
          else do
            di <- padj prettyIdent i
            de <- padj portexpr l
            mkid $ ng $ dot <> di <> gpar de
  where
    pst (Identified i x) = prettySpecTerm $ SpecTerm i x
    portexpr :: PrettyIdent [Identified (Maybe (RangeExpr CExpr CExpr))]
    portexpr l = case l of
      [] -> pure (mempty, mempty)
      [x] -> pst x
      _ -> cslid (lbrace <> softspace) rbrace pst l >>= mkid

prettyModuleBlock ::
  LocalCompDir -> ModuleBlock NExpr CExpr MPExpr -> Reader PrintingOpts (Doc, LocalCompDir)
prettyModuleBlock (LocalCompDir ts c p dn) (ModuleBlock a mm i pi b mts mc mp mdn) = do
  head <- fpadj (group . ((if mm then "macromodule" else "module") <=>)) prettyIdent i
  ports <- prettyPortInter pi
  header <- prettyItem a $ head <> gpar ports
  body <- prettyModuleItems b
  let
    dts =
      piff (let (a, b) = fromJust mts in "`timescale" <+> tsval a <+> "/" <+> tsval b) (ts == mts)
    dcd = piff (if mc then "`celldefine" else "`endcelldefine") (c == mc)
    dud =
      piff
        ( case mp of
            Nothing -> "`nounconnected_drive"
            Just b -> "`unconnected_drive pull" <> if b then "1" else "0"
        )
        (p == mp)
    ddn = piff ("`default_nettype" <+> maybe "none" viaShow mdn) (dn == mdn)
  return
    (dts <?#> dcd <?#> dud <?#> ddn <?#> block header "endmodule" body, LocalCompDir mts mc mp mdn)
  where
    tsval i =
      let (u, v) = divMod i 3
       in case v of 0 -> "1"; 1 -> "10"; 2 -> "100"
            <> case u of 0 -> "s"; -1 -> "ms"; -2 -> "us"; -3 -> "ns"; -4 -> "ps"; -5 -> "fs"

prettyPrimPorts :: (Attributes, PrimPort CExpr, NonEmpty Identifier) -> Print
prettyPrimPorts (a, d, l) = do
  (ids, s) <- cslid1 prettyIdent l
  ports <- case d of
    PPOutReg (Just e) -> (\x -> ids <=> equals <+> x) <$> gpadj prettyCExpr e
    _ -> pure $ ids <> s
  prettyItem a $
    (case d of PPInput -> "input"; PPOutput -> "output"; PPReg -> "reg"; PPOutReg _ -> "output reg")
    <=> ports

-- | Analyses the column size of all lines and returns a list of either
-- | number of successive non-edge columns or the width of a column that contain edges
seqrowAlignment :: NonEmpty SeqRow -> [Either Int Int]
seqrowAlignment =
  maybe [] fst . foldrMapM1
    ( \sr@(SeqRow sri _ _) -> Just (case sri of
        SISeq l0 e l1 -> [Left $ length l0, Right $ edgeprintsize e, Left $ length l1]
        SIComb l -> [Left $ length l],
      totallength sr)
    )
    ( \sr@(SeqRow sri _ _) (sl, tl) ->
        if totallength sr /= tl
          then Nothing
          else case sri of
            SISeq l0 e l1 -> (,) <$> spliceputat (edgeprintsize e) (length l0) sl <*> pure tl
            SIComb l -> Just (sl, tl)
    )
  where
    totallength sr =
      case _srowInput sr of SIComb l -> length l; SISeq l0 e l1 -> 1 + length l0 + length l1
    edgeprintsize x = case x of EdgePos_neg _ -> 1; EdgeDesc _ _ -> 4
    -- Searches and splices a streak of non edge columns and puts an edge column
    spliceputat w n l = case l of
      [] -> Nothing
      Right w' : t ->
        if 0 < n
          then (Right w' :) <$> spliceputat w (n - 1) t
          else Just $ Right (max w w') : t
      Left m : t -> case compare m (n + 1) of
        LT -> (Left m :) <$> spliceputat w (n - m) t
        EQ -> Just $ Left n : Right w : t
        GT -> Just $ Left n : Right w : Left (m - n - 1) : t

-- | Prints levels or edges in an aligned way using analysis results
-- | the second part of the accumulation is Right if there is an edge, Left otherwise
prettySeqIn :: SeqIn -> [Either Int Int] -> Print
prettySeqIn si l = do
  ts <- asks _poTableSpace
  let pcat f = foldrMap1' mempty f ((if ts then (<+>) else (<>)) . f)
  return $
    fst $
      foldl'
        ( \(d, si) e -> case (e, si) of
            (Left n, Left l') ->
              let (l0, l1) = splitAt n l'
               in (d <> pcat viaShow l0, Left l1)
            (Right w, Left (h : t)) -> (d <> prettylevelwithwidth h w, Left t)
            (Left n, Right (l0, e, l1)) ->
              let (l00, l01) = splitAt n l0
               in (d <> pcat viaShow l00, Right (l01, e, l1))
            (Right w, Right ([], e, l1)) ->
              (d <> viaShow e <> pift sp3 (edgeprintsize e < w), Left l1)
            (Right w, Right (h : t, e, l')) -> (d <> prettylevelwithwidth h w, Right (t, e, l'))
        )
        (mempty, case si of SIComb l -> Left $ NE.toList l; SISeq a b c -> Right (a, b, c))
        l
  where
    edgeprintsize x = case x of EdgePos_neg _ -> 1; EdgeDesc _ _ -> 4
    sp3 = raw "   "
    prettylevelwithwidth l w = viaShow l <> pift sp3 (w == 4)
    

-- | Prints a table in a singular block with aligned columns, or just prints it if not possible
prettySeqRows :: NonEmpty SeqRow -> Print
prettySeqRows l = do
  ts <- asks _poTableSpace
  let
    pp :: (Foldable t, Show x) => t x -> Doc
    pp l =
      raw $
        fromString $
          if ts then intercalate " " $ map show $ toList l
          else concatMap show l
    psi x = case x of
      SIComb l -> pp l
      SISeq l0 e l1 -> pp l0 <+> viaShow e <+> pp l1
  pl
    (<#>)
    ( case seqrowAlignment l of
        -- fallback prettyprinting because aligned is better but not always possible
        [] -> \(SeqRow si s ns) -> pure $ nest $ psi si <=> prettyend s ns
        -- aligned prettyprinting
        colws -> \(SeqRow si s ns) -> nest . (<=> prettyend s ns) <$> prettySeqIn si colws
    )
    l
  where
    prettyend s ns = group (colon <+> viaShow s <=> colon <+> maybe "-" viaShow ns) <> semi

prettyPrimTable :: Doc -> PrimTable -> Print
prettyPrimTable od b = case b of
  CombTable l -> do
    ts <- asks _poTableSpace
    let
      pp l =
        if ts then intercalate " " $ map show $ toList l
        else concatMap show l
    return $
      block "table" "endtable" $
        (\f -> foldrMap1 f ((<#>) . f))
          (\(CombRow i o) -> nest $ raw (fromString $ pp i) <=> colon <+> viaShow o <> semi)
          l
  SeqTable mi l -> do
    table <- prettySeqRows l
    let pinit iv = case iv of ZOXX -> "1'bx"; ZOXZ -> "0"; ZOXO -> "1"
    return $
      maybe mempty (\iv -> nest $ group ("initial" <=> od) <=> equals <+> pinit iv <> semi) mi
        <?#> block "table" "endtable" table

prettyPrimitiveBlock :: PrimitiveBlock CExpr -> Print
prettyPrimitiveBlock (PrimitiveBlock a s o i pd b) = do
  (od, ol) <- prettyIdent o
  head <- fpadj (group . ("primitive" <=>)) prettyIdent s
  ports <- padj (cslid1 prettyIdent) i
  header <- prettyItem a $ head <> gpar (od <> ol <.> ports)
  table <- prettyPrimTable od b
  block header "endprimitive" . (<#> table)
    <$> prettyregroup
      (<#>)
      prettyPrimPorts
      (\(AttrIded a i p) -> (a, p, [i]))
      ( \(AttrIded na i np) (a, p, l) -> case (np, p) of
          (PPInput, PPInput) | na == a -> Just (a, p, i <| l)
          _ -> Nothing
      )
      pd

prettyConfigItem :: ConfigItem -> Print
prettyConfigItem (ConfigItem ci llu) = do
  dci <- case ci of
    CICell c -> ("cell" <=>) . fst <$> prettyDot1Ident c
    CIInst i ->
      ("instance" <=>)
        <$> foldrMap1 (fmap fst . prettyIdent) (liftA2 (\a b -> a <> dot <> b) . padj prettyIdent) i
  dllu <- case llu of
    LLULiblist ls -> ("liblist" <?=>) <$> catid prettyBS ls
    LLUUse s c -> (\x -> "use" <=> x <> pift ":config" c) <$> padj prettyDot1Ident s
  return $ nest $ ng dci <=> ng dllu <> semi
  where
    catid f = foldrMap1' (pure mempty) (gpadj f) $ liftA2 ((<=>) . fst) . f

prettyConfigBlock :: ConfigBlock -> Print
prettyConfigBlock (ConfigBlock i des b def) = do
  di <- padj prettyIdent i
  design <- catid prettyDot1Ident des
  body <- pl (<#>) prettyConfigItem b
  dft <- catid prettyBS def
  return $
    block (nest $ "config" <=> di <> semi) "endconfig" $
      nest ("design" <?=> design) <> semi <#> body <?#> nest ("default liblist" <?=> dft) <> semi
  where
    catid f = foldrMap1' (pure mempty) (gpadj f) $ liftA2 ((<=>) . fst) . f

prettyVerilog2005 :: Verilog2005 -> Print
prettyVerilog2005 (Verilog2005 mb pb cb) = do
  mods <-
    foldl'
      ( \macc m -> do
          (d, lcd) <- macc
          first (d <##>) <$> prettyModuleBlock lcd m
      )
      (pure (mempty, lcdDefault))
      mb
  prims <- pl (<##>) prettyPrimitiveBlock pb
  confs <- pl (<##>) prettyConfigBlock cb
  return $ fst mods <##> prims <##> confs
  where
    (<##>) = mkopt $ \a b -> a <#> mempty <#> b
