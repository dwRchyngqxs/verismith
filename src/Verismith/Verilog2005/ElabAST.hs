-- Module      : Verismith.Verilog2005.ElabAST
-- Description : Verilog 2005 Elaboration AST.
-- Copyright   : (c) 2024 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Verismith.Verilog2005.ElabAST
  ( ElabType (..),
    ElabBinaryOperator (..),
    ElabPrim (..),
    ElabExpr (..),
    ElabGenCaseItem (..),
    ElabModGenItem (..),
    ElabModuleItem (..),
    ElabGenerateBlock (..),
    ElabModuleBlock (..),
    ElabVerilog2005 (..),
    elabPrim,
    elabExpr,
    elabBinary,
    elabBinaryUnlimited,
    elabOctal,
    elabOctalUnlimited,
    elabHexadecimal,
    elabHexadecimalUnlimited,
    elabNumber,
    elabString,
  )
where

import GHC.Generics (Generic)
import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data
import Data.Data.Lens
import Numeric.Natural
import Verismith.Verilog2005.Utils (fitSnSz)
import Verismith.Verilog2005.AST

-- | Expression types
data ElabType
  = ETUnknown
  | ETNothing
  | ETReal
  | ETBitVector
    { _etbvSign :: !Bool,
      _etbvSize :: !Natural -- 0 means infinite
    }
  deriving (Data, Generic)

isBV :: ElabType -> Bool
isBV t = case t of ETUnknown -> True; ETBitVector _ _ -> True; _ -> False

isFinBV :: ElabType -> Bool
isFinBV t = case t of ETUnknown -> True; ETBitVector _ sz -> sz <> 0; _ -> False

isInt :: ElabType -> Bool
isInt t = case t of ETUnknown -> True; ETBitVector True 0 -> True; _ -> False

isReal :: ElabType -> Bool
isReal t = case t of ETUnknown -> True; ETReal -> True; _ -> False

-- | Binary operators
data ElabBinaryOperator
  = EBPlus !ElabType
  | EBMinus !ElabType
  | EBTimes !ElabType
  | EBDiv !ElabType
  | EBMod !ElabType
  | EBEq !ElabType
  | EBNEq !ElabType
  | EBCEq !ElabType
  | EBCNEq !ElabType
  | EBLAnd
  | EBLOr
  | EBLT !ElabType
  | EBLEq !ElabType
  | EBGT !ElabType
  | EBGEq !ElabType
  | EBAnd !ElabType
  | EBOr !ElabType
  | EBXor !ElabType
  | EBXNor !ElabType
  | EBPower !ElabType
  | EBLSL
  | EBLSR
  | EBASL
  | EBASR
  deriving (Data, Generic)

-- | Parametric primary expression
data ElabPrim i r a
  = EPNumber
      { _epnSize :: !Natural, -- 0 means unspecified
        _epnSigned :: !Bool,
        _epnValue :: !Integer,
        _epnXZMask :: !Integer
      }
  | EPReal !Double
  | EPIdent
      { _epiType :: !ElabType,
        _epiIdent :: !i,
        _epiSub :: !r
      }
  | EPConcat
      { _epcSize :: !(Maybe Natural),
        _epcArgs :: !(NonEmpty (ElabExpr i r a))
      }
  | EPMultConcat
      { _epmcSize :: !(Maybe Natural),
        _epmcMul :: !(ElabExpr Identifier (Maybe (RangeExpr ElabCExpr ElabCExpr)) a),
        _epmcExpr :: !(NonEmpty (ElabExpr i r a))
      }
  | EPFun
      { _epfIdent :: !i,
        _epfAttr :: !a,
        _epfArgs :: ![ElabExpr i r a]
      }
  | EPSysFun
      { _epsfType :: !ElabType,
        _epsfIdent :: !(Either SystemFunction ByteString),
        _epsfArgs :: ![ElabExpr i r a]
      }
  | EPMinTypMax
      { _epmtmType :: !ElabType,
        _epmtmVal :: !(MinTypMax (ElabExpr i r a))
      }
  deriving (Data, Generic)

-- | Parametric expression
data ElabExpr i r a
  = EEPrim !(ElabPrim i r a)
  | EEUnOp
      { _eeuOp :: !UnaryOperator,
        _eeuAttr :: !a,
        _eeuPrim :: !(ElabPrim i r a)
      }
  | EEBinOp
      { _eebLhs :: !(ElabExpr i r a),
        _eebOp :: !ElabBinaryOperator,
        _eebAttr :: !a,
        _eebRhs :: !(ElabExpr i r a)
      }
  | EECond
      { _eecType :: !ElabType,
        _eecCond :: !(ElabExpr i r a),
        _eecAttr :: !a,
        _eecTrue :: !(ElabExpr i r a),
        _eecFalse :: !(ElabExpr i r a)
      }
  deriving (Data, Generic)

newtype ElabCExpr
  = ElabCExpr (ElabExpr Identifier (Maybe (RangeExpr ElabCExpr ElabCExpr)) Attributes)
  deriving (Data, Generic)

newtype ElabNExpr
  = ElabNExpr (ElabExpr (HierIdent ElabCExpr) (Maybe (DimRange ElabNExpr ElabCExpr)) Attributes)
  deriving (Data, Generic)

-- | Case generate branch
data ElabGenCaseItem = GenCaseItem
  { _egciPat :: !(NonEmpty ElabCExpr), -- Maybe not that?
    _egciVal :: !ElabGenerateCondBlock
  }
  deriving (Data, Generic)

-- | Module or Generate conditional item because scoping rules are special
data ElabModGenCondItem
  = EMGCIIf
      { _emgiiExpr :: !ElabCExpr,
        _emgiiTrue :: !ElabGenerateCondBlock,
        _emgiiFalse :: !ElabGenerateCondBlock
      }
  | EMGCICase
      { _emgicExpr :: !ElabCExpr,
        _emgicBranch :: ![ElabGenCaseItem],
        _emgicDefault :: !ElabGenerateCondBlock
      }
  deriving (Data, Generic)

-- | Generate Block or Conditional Item or nothing because scoping rules are special
data ElabGenerateCondBlock
  = EGCBEmpty
  | EGCBBlock !ElabGenerateBlock
  | EGCBConditional !(Attributed ElabModGenCondItem)
  deriving (Data, Generic)

-- | Module or Generate item
data ElabModGenItem
  = EMGINetInit
      { _emginiType :: !NetType,
        _emginiDrive :: !DriveStrength,
        _emginiProp :: !(NetProp ElabNExpr ElabCExpr),
        _emginiInit :: !(NetInit ElabNExpr)
      }
  | EMGINetDecl
      { _emgindType :: !NetType,
        _emgindProp :: !(NetProp ElabNExpr ElabCExpr),
        _emgindDecl :: !(NetDecl ElabCExpr)
      }
  | EMGITriD
      { _emgitdDrive :: !DriveStrength,
        _emgitdProp :: !(NetProp ElabNExpr ElabCExpr),
        _emgitdInit :: !(NetInit ElabNExpr)
      }
  | EMGITriC
      { _emgitcCharge :: !ChargeStrength,
        _emgitcProp :: !(NetProp ElabNExpr ElabCExpr),
        _emgitcDecl :: !(NetDecl ElabCExpr)
      }
  | EMGIBlockDecl !(BlockDecl Identified (Either [Range2 ElabCExpr] ElabCExpr) ElabCExpr)
  | EMGIGenVar !Identifier
  | EMGITask
      { _emgitAuto :: !Bool,
        _emgitIdent :: !Identifier,
        _emgitDecl :: ![AttrIded (TFBlockDecl Dir ElabCExpr)],
        _emgitBody :: !(Attributed (Maybe (Statement ElabNExpr ElabCExpr)))
      }
  | EMGIFunc
      { _emgifAuto :: !Bool,
        _emgifType :: !(Maybe (ComType () ElabCExpr)),
        _emgifIdent :: !Identifier,
        _emgifDecl :: ![AttrIded (TFBlockDecl () ElabCExpr)],
        _emgifBody :: !(FunctionStatement ElabNExpr ElabCExpr)
      }
  | EMGIDefParam !(ParamOver ElabCExpr)
  | EMGIContAss
      { _emgicaStrength :: !DriveStrength,
        _emgicaDelay :: !(Maybe (Delay3 ElabNExpr)),
        _emgicaAssign :: !(Assign ElabCExpr ElabCExpr ElabNExpr)
      }
  | EMGIGate !(Gate Identity ElabNExpr ElabCExpr)
  | EMGIUDPInst
      { _emgiudpiUDP :: !Identifier,
        _emgiudpiStrength :: !DriveStrength,
        _emgiudpiDelay :: !(Maybe (Delay2 ElabNExpr)),
        _emgiudpiInst :: !(UDPInst ElabNExpr ElabCExpr)
      }
  | EMGIModInst
      { _emgimiMod :: !Identifier,
        _emgimiParams :: !(ParamAssign ElabNExpr),
        _emgimiInst :: !(ModInst ElabNExpr ElabCExpr)
      }
  | EMGIUnknownInst
      { _emgiuiType :: !Identifier,
        _emgiuiParam :: !(Maybe (Either ElabNExpr (ElabNExpr, ElabNExpr))),
        _emgiuiInst :: !(UknInst ElabNExpr ElabCExpr)
      }
  | EMGIInitial !(Attributed (Statement ElabNExpr ElabCExpr))
  | EMGIAlways !(Attributed (Statement ElabNExpr ElabCExpr))
  | EMGILoopGen
      { _emgilgInitIdent :: !Identifier,
        _emgilgInitValue :: !ElabCExpr,
        _emgilgCond :: !ElabCExpr,
        _emgilgUpdIdent :: !Identifier,
        _emgilgUpdValue :: !ElabCExpr,
        _emgilgBody :: !ElabGenerateBlock
      }
  | EMGICondItem !ElabModGenCondItem
  | EMGIGenerateBlock
      { _emgigbIdent :: !Identifier,
        _emgigbIndex :: ![Integer],
        _emgigbBody :: ![Attributed ElabModGenItem]
      }
  | EMGIResolvedMod
      { _emgirmInst :: !(ModInst ElabNExpr ElabCExpr),
        _emgimiModule :: !ElabModuleBlock
      }
  deriving (Data, Generic)

instance Plated ElabModGenItem where
  plate = uniplate

-- | Module item: body of module
-- | Caution: if MIPort sign is False then it can be overriden by a MGINetDecl/Init
data ElabModuleItem
  = EMIMGI !(Attributed ElabModGenItem)
  | EMIPort !(AttrIded (Dir, SignRange ElabCExpr))
  | EMIParameter !(AttrIded (Parameter ElabCExpr))
  | EMIGenReg ![Attributed ElabModGenItem]
  | EMISpecParam
    { _emispAttribute :: !Attributes,
      _emispRange :: !(Maybe (Range2 ElabCExpr)),
      _emispDecl :: !(SpecParamDecl ElabCExpr)
    }
  | EMISpecBlock ![SpecifyBlockedItem ElabNExpr ElabCExpr (ElabExpr Identifier () Attributes)]
  deriving (Data, Generic)

data ElabGenerateBlock = ElabGenerateBlock
  { _egbIdent :: !(Maybe Identifier),
    _egbBody :: ![Attributed ElabModGenItem]
  }
  deriving (Data, Generic)

-- | Module block
data ElabModuleBlock = ElabModuleBlock
  { _embAttr :: !Attributes,
    _embMacro :: !Bool,
    _embIdent :: !Identifier,
    _embPortInter :: ![Identified [Identified (Maybe (RangeExpr ElabCExpr ElabCExpr))]],
    _embBody :: ![ElabModuleItem],
    _embTimescale :: !(Maybe (Int, Int)),
    _embCell :: !Bool,
    _embPull :: !(Maybe Bool),
    _embDefNetType :: !(Maybe NetType)
  }
  deriving (Data, Generic)

-- | Internal representation of Verilog2005 AST
data ElabVerilog2005 = ElabVerilog2005
  { _evModule :: ![ElabModuleBlock],
    _evPrimitive :: ![PrimitiveBlock ElabCExpr],
    _evConfig :: ![ConfigBlock]
  }
  deriving (Data, Generic)

$(makeLenses ''ElabModuleBlock)

-- | Retreive the type of a (partially) elaborated primary
elabPrimGetType :: ElabPrim i r a -> ElabType
elabPrimGetType x = case x of
  EPNumber sz sn _ _ -> ETBitVector sn sz
  EPReal _ -> ETReal
  EPIdent t _ _ -> t
  EPConcat sz _ -> maybe ETUnknown (\s -> if s == 0 then ETNothing else ETBitVector False s) sz
  EPMultConcat sz _ _ ->
    maybe ETUnknown (\s -> if s == 0 then ETNothing else ETBitVector False s) sz
  EPFun i _ a -> ETUnknown
  EPSysFun t _ _ -> t
  EPMinTypMax t _ -> t

-- | Retreive the type of a (partially) elaborated expression
elabExprGetType :: ElabExpr i r a -> ElabType
elabExprGetType x = case x of
  EEPrim p -> elabPrimGetType p
  EEUnOp o _ p -> case o of
    UnPlus -> case elabPrimGetType p of
      ETNothing -> err
      t -> t
    UnMinus -> case elabPrimGetType p of
      ETNothing -> err
      t -> t
    UnLNot -> ETBitVector False 1
    UnNot -> noRealType $ elabPrimGetType p
    UnAnd -> ETBitVector False 1
    UnNand -> ETBitVector False 1
    UnOr -> ETBitVector False 1
    UnNor -> ETBitVector False 1
    UnXor -> ETBitVector False 1
    UnXNor -> ETBitVector False 1
  EEBinOp l o _ _ -> case o of
    EBPlus t -> t
    EBMinus t -> t
    EBTimes t -> t
    EBDiv t -> t
    EBMod t -> t
    EBEq _ -> ETBitVector False 1
    EBNEq _ -> ETBitVector False 1
    EBCEq _ -> ETBitVector False 1
    EBCNEq _ -> ETBitVector False 1
    EBLAnd -> ETBitVector False 1
    EBLOr -> ETBitVector False 1
    EBLT _ -> ETBitVector False 1
    EBLEq _ -> ETBitVector False 1
    EBGT _ -> ETBitVector False 1
    EBGEq _ -> ETBitVector False 1
    EBAnd t -> t
    EBOr t -> t
    EBXor t -> t
    EBXNor t -> t
    EBPower t -> t
    EBLSL -> noRealType $ elabExprGetType l
    EBLSR -> noRealType $ elabExprGetType l
    EBASL -> noRealType $ elabExprGetType l
    EBASR -> noRealType $ elabExprGetType l
  EECond t _ _ _ _ -> t
  where
    err = errro "Invalid expression type"
    noRealType t = case t of
      ETReal -> err
      ETNothing -> err
      _ -> t

-- | Evaluates a binary number
evalBinaryUnlimited :: NonEmpty BXZ -> (Integer, Integer)
evalBinaryUnlimited (h :| t) =
  foldl'
    add
    (case h of BXZ0 -> (0, 0); BXZ1 -> (1, 0); BXZX -> (-1, -1); BXZZ -> (0, -1))
    t
  where
    add (v, m) x = let (b, n) = binval x in (b .|. (v .<<. 1), n .|. (m .<<. 1))
    binval x = case x of BXZ0 -> (0, 0); BXZ1 -> (1, 0); BXZX -> (1, 1); BXZZ -> (0, 1)

-- | Evaluates a binary number of a given size
evalBinary :: Natural -> NonEmpty BXZ -> (Integer, Integer)
evalBinary sz l = case NE.drop szdiff l of
  [] -> error "non-positive sized literal"
  h : t -> evalBinaryUnlimited $ h :| t
  where szdiff = fromEnum $ toEnum (length l) - sz

-- | Evaluates a list of octal digits
evalOctalCommon :: (Integer, Integer) -> [OXZ] -> (Integer, Integer)
evalOctalCommon = foldl' add
  where
    add (v, m) x = let (b, n) = octval x in (b .|. (v .<<. 3), n .|. (m .<<. 3))
    octval x = case x of
      OXZ0 -> (0, 0)
      OXZ1 -> (1, 0)
      OXZ2 -> (2, 0)
      OXZ3 -> (3, 0)
      OXZ4 -> (4, 0)
      OXZ5 -> (5, 0)
      OXZ6 -> (6, 0)
      OXZ7 -> (7, 0)
      OXZX -> (7, 7)
      OXZZ -> (0, 7)

-- | Evaluates an octal number
evalOctalUnlimited :: NonEmpty OXZ -> (Integer, Integer)
evalOctalUnlimited (h :| t) =
  evalOctalCommon
    ( case h of
        OXZ0 -> (0, 0)
        OXZ1 -> (1, 0)
        OXZ2 -> (2, 0)
        OXZ3 -> (3, 0)
        OXZ4 -> (4, 0)
        OXZ5 -> (5, 0)
        OXZ6 -> (6, 0)
        OXZ7 -> (7, 0)
        OXZX -> (-1, -1)
        OXZZ -> (0, -1)
    )
    t

-- | Evaluates an octal number of a given size
evalOctal :: Natural -> NonEmpty OXZ -> (Integer, Integer)
evalOctal sz l = case NE.drop szdiff l of
  [] -> error "non-positive sized literal"
  h : t ->
    evalOctalCommon
      ( case h of
          OXZ0 -> (0, 0)
          OXZ1 -> (1, 0)
          OXZ2 -> (2, 0)
          OXZ3 -> (3, 0)
          OXZ4 -> (4, 0)
          OXZ5 -> (5, 0)
          OXZ6 -> (6, 0)
          OXZ7 -> (7, 0)
          OXZX -> (bm, bm)
          OXZZ -> (0, bm)
      )
      t
  where
    szdiff = fromEnum $ toEnum (length l) - div (sz + 2) 3 -- +2 to round up
    bm = bit (fromEnum $ if 0 < szdiff then 1 + mod (sz + 2) 3 else sz - 3*toEnum (length l - 1)) - 1

-- | Evaluates a list of hexadecimal digits
evalHexadecimalCommon :: (Integer, Integer) -> [HXZ] -> (Integer, Integer)
evalHexadecimalCommon = foldl' add
  where
    add (v, m) x = let (b, n) = hexval x in (b .|. (v .<<. 4), n .|. (m .<<. 4))
    hexval x = case x of
      HXZ0 -> (0, 0)
      HXZ1 -> (1, 0)
      HXZ2 -> (2, 0)
      HXZ3 -> (3, 0)
      HXZ4 -> (4, 0)
      HXZ5 -> (5, 0)
      HXZ6 -> (6, 0)
      HXZ7 -> (7, 0)
      HXZ8 -> (8, 0)
      HXZ9 -> (9, 0)
      HXZA -> (10, 0)
      HXZB -> (11, 0)
      HXZC -> (12, 0)
      HXZD -> (13, 0)
      HXZE -> (14, 0)
      HXZF -> (15, 0)
      HXZX -> (15, 15)
      HXZZ -> (0, 15)

-- | Evaluates a hexadecimal number
evalHexadecimalUnlimited :: NonEmpty HXZ -> (Integer, Integer)
evalHexadecimalUnlimited (h :| t) =
  evalHexadecimalCommon
    ( case h of
        HXZ0 -> (0, 0)
        HXZ1 -> (1, 0)
        HXZ2 -> (2, 0)
        HXZ3 -> (3, 0)
        HXZ4 -> (4, 0)
        HXZ5 -> (5, 0)
        HXZ6 -> (6, 0)
        HXZ7 -> (7, 0)
        HXZ8 -> (8, 0)
        HXZ9 -> (9, 0)
        HXZA -> (10, 0)
        HXZB -> (11, 0)
        HXZC -> (12, 0)
        HXZD -> (13, 0)
        HXZE -> (14, 0)
        HXZF -> (15, 0)
        HXZX -> (-1, -1)
        HXZZ -> (0, -1)
    )
    t

-- | Evaluates a hexadecimal number of a given size
evalHexadecimal :: Natural -> NonEmpty HXZ -> (Integer, Integer)
evalHexadecimal sz l = case NE.drop szdiff l of
  [] -> error "non-positive sized literal"
  h : t ->
    evalHexadecimalCommon
      ( case h of
          HXZ0 -> (0, 0)
          HXZ1 -> (1, 0)
          HXZ2 -> (2, 0)
          HXZ3 -> (3, 0)
          HXZ4 -> (4, 0)
          HXZ5 -> (5, 0)
          HXZ6 -> (6, 0)
          HXZ7 -> (7, 0)
          HXZ8 -> (8, 0)
          HXZ9 -> (9, 0)
          HXZA -> (10, 0)
          HXZB -> (11, 0)
          HXZC -> (12, 0)
          HXZD -> (13, 0)
          HXZE -> (14, 0)
          HXZF -> (15, 0)
          HXZX -> (bm, bm)
          HXZZ -> (0, bm)
      )
      t
  where
    szdiff = fromEnum $ toEnum (length l) - div (sz + 3) 4 -- +3 to round up
    bm = bit (fromEnum $ if 0 < szdiff then 1 + mod (sz + 3) 3 else sz - 4*toEnum (length l - 1)) - 1

-- | Evaluates a primary expression number
evalNumber :: Natural -> Bool -> Number -> (Integer, Integer)
evalNumber sz sn v = case v of
    NBinary l | sz == 0 -> evalBinaryUnlimited l
    NBinary l -> evalBinary sz l
    NOctal l | sz == 0 -> evalOctalUnlimited l
    NOctal l -> evalOctal sz l
    NDecimal n -> (toInteger n, 0)
    NHex l | sz == 0 -> evalHexadecimalUnlimited l
    NHex l -> evalHexadecimal sz l
    NXZ x_z -> (if x_z then -1 else 0, -1)

-- | Evaluates a primary expression string
-- | this looks too complex for what it does, blame escape sequences and folds
evalString :: BS.ByteString -> (Natural, Integer)
evalString s = (max 8 (sz .<<. 3), n)
  where
    isOctalDigit c = c2w '0' <= c && c < c2w '8'
    mkEscape l =
      let n = foldr (\n a -> (a .<<. 3) .|. toInteger n) 0 l
       in if n <= 255 then n else error "Maximum character code is 377 (octal)"
    (tn, tsz, tes) =
      BS.foldl'
        ( \(n, sz, es) c -> case es of
            Just [] -> case w2c c of
              'n' -> ((n .<<. 8) .|. toInteger (fromEnum '\n'), sz + 1, Nothing)
              't' -> ((n .<<. 8) .|. toInteger (fromEnum '\t'), sz + 1, Nothing)
              '\\' -> ((n .<<. 8) .|. toInteger (fromEnum '\\'), sz + 1, Nothing)
              '"' -> ((n .<<. 8) .|. toInteger (fromEnum '"'), sz + 1, Nothing)
              _ | isOctalDigit c -> (n, sz, Just [fromEnum $ c - c2w '0'])
              _ -> error "Invalid character in escape sequence"
            Just code | isOctalDigit c ->
              let ncode = fromEnum (c - c2w '0') : code
               in if length code == 2
                    then ((n .<<. 8) .|. mkEscape ncode, sz + 1, Nothing)
                    else (n, sz, Just ncode)
            Nothing | c == c2w '\\' -> (n, sz, Just [])
            Just code | c == c2w '\\' -> ((n .<<. 8) .|. mkEscape code, sz + 1, Just [])
            Nothing -> ((n .<<. 8) .|. toInteger c, sz + 1, Nothing)
            Just code -> ((n .<<. 16) .|. (mkEscape code .<<. 8) .|. toInteger c, sz + 2, Nothing)
        )
        (0, 0, Nothing)
        s
    (n, sz) = maybe (tn, tsz) (\l -> ((tn .<<. 8) .|. mkEscape l, tsz + 1)) tes

-- | Rule for merging types according to the standard
elabMaxType :: ElabType -> ElabType -> ElabType
elabMaxType t1 t2 = case (t1, t2) of
  (ETNothing, ETNothing) -> ETNothing
  (ETNothing, _) -> err
  (_, ETNothing) -> err
  (ETUnknown, _) -> ETUnknown
  (_, ETUnknown) -> ETUnknown
  (ETReal, _) -> ETReal
  (_, ETReal) -> ETReal
  (ETBitVector sn1 0, ETBitVector sn2 _) -> ETBitVector (sn1 && sn2) 0
  (ETBitVector sn1 _, ETBitVector sn2 0) -> ETBitVector (sn1 && sn2) 0
  (ETBitVector sn1 sz1, ETBitVector sn2 sz2) -> ETBitVector (sn1 && sn2) (max sz1 sz2)
  where err = error "Cannot merge empty concatenation type"

-- | Changing size while preserving signedness
elabResize :: ElabType -> ElabType -> ElabType
elabResize ot nt = case (ot, nt) of
  (ETNothing, _) -> err
  (_, ETNothing) -> err
  (ETUnknown, _) -> ETUnknown
  (_, ETUnknown) -> ETUnknown
  (ETReal, _) -> ETReal
  (_, ETReal) -> ETReal
  (ETBitVector sn _, ETBitVector _ sz) -> ETBitVector sn sz
  where err = error "Attempt at resizing a self-determined empty concatenation"

-- DOC: more precision on typing rules: https://accellera.mantishub.io/view.php?id=1072
-- | (Partially-)elaborates a primary assuming a self-determined context
elabPrim1 :: (i -> ei) -> (r -> er) -> Bool -> Prim i r a -> (ElabPrim ei er a, ElabType)
elabPrim1 fi fr partial x = case x of
  PrimNumber sz sn n -> (uncurry (EPNumber sz sn) $ elabNumber sz sn n, ETBitVector sn sz)
  PrimReal s -> (EPReal $ read $ unpackChars s, ETReal)
  PrimIdent i r -> (EPIdent ETUnknown (fi i) (fr r), ETUnknown)
  PrimConcat l -> case mkCat l of
    (ll, Nothing) -> (EPConcat Nothing ll, ETUnknown)
    (ll, Some sz) -> (EPConcat (Some sz) ll, ETBitVector False sz)
  PrimMultConcat n l ->
    let (nn, nt) = elabExprSelfDet id (maybe $ bimapRangeExpr elabCExprSelfDet elabCExprSelfDet) n
     in case evalExpr nn of
        Nothing | nt <> ETNothing -> (EPMultConcat Nothing nn $ fst $ mkCat l, ETUnknown)
        Just (VInt sn sz v 0) ->
          let mn = EEPrim $ EPNumber sz sn v 0
              tval = fitSnSz sn sz v
           in case compare tval 0 of
              LT -> caterr
              EQ -> (EPMultConcat (Some 0) mn [EEPrim $ EPNumber False 1 0], ETNothing)
              GT -> case mkCat l of
                (ll, Nothing) -> (EPMultConcat Nothing mn ll, ETUnknown)
                (ll, Some ssz) -> let nsz = ssz * tval in
                  (EPMultConcat (Some nsz) mn ll, ETBitVector False nsz)
        _ -> caterr
  PrimFun i attr args -> (EPFun (fi i) attr $ fst . mkSD <$> args, ETUnknown)
  PrimSysFun s args ->
    maybe (EPSysFun ETUnknown (Right s) $ fst . mkSD <$> args, ETUnknown) (mkSF args) $
      HashMap.lookup s sfMap
  PrimMinTypMax (MTMSingle e) -> let (ne, t) = mkE e in (EPMinTypMax t $ MTMSingle ne, t)
  PrimMinTypMax (MTMFull em et eM) ->
    let (nem, tm) = mkE em
        (net, tt) = mkE et
        (neM, tM) = mkE eM
        t = elabMaxType (elabMaxType tm tt) tM
     in (EPMinTypMax t $ MTMFull nem net neM, t)
  PrimString b -> let (sz, v) = evalString b in (EPNumber False sz v 0, ETBitVector False sz)
  where
    caterr = error "Error in concatenation"
    sferr = error "Invalid arguments to system function"
    mkSD = elabExprSelfDet fi fr
    mkE = (if partial then elabExprCtxDet1 else elabExprSelfDet) fi fr
    auxCat (al, at) (x, t) = case (at, t) of
      (_, ETNothing) -> (al, at)
      (_, ETUnknown) -> (al : x, Nothing)
      (Nothing, t) | isFinBV t -> (al : x, Nothing)
      (Just sz, ETBitVector _ nsz) | nsz <> 0 -> (al : x, Some $ sz + nsz)
      _ -> caterr
    mkCat l = case fold auxCat ([], Some 0) $ mkE <$> l of
      (h : t, Nothing) -> (h :| t, Nothing)
      (h : t, Some sz) | sz <> 0 -> (h :| t, Some sz)
      _ -> caterr
    mkSF l sf = case (sf, mkE <$> l) of -- integer is considered signed with unlimited bits
      (SFFscanf, [(fd, fdt), (fmt, fmtt), (a, t)]) | isBV fd && isFinBV fmtt && t <> ETNothing ->
        (EPSysFun (ETBitVector True 0) (Left SFFscanf) [fd, fmt, a], ETBitVector True 0)
      (SFFread, [(m, mt), (fd, fdt)]) | isFinBV mt && isBV fdt ->
        (EPSysFun (ETBitVector True 0) (Left SFFread) [m, fd], ETBitVector True 0)
      (SFFread, [(m, mt), (fd, fdt), (s, st)]) | isFinBV mt && isBV fdt && isBV st ->
        (EPSysFun (ETBitVector True 0) (Left SFFread) [m, fd, s], ETBitVector True 0)
      (SFFread, [(m, mt), (fd, fdt), (s, st), (c, ct)])
        | isFinBV mt && isBV fdt && isBV st && isBV ct ->
        (EPSysFun (ETBitVector True 0) (Left SFFread) [m, fd, s, c], ETBitVector True 0)
      (SFFseek, [(fd, fdt), (off, offt), (op, opt)]) | isBV fdt && isBV offt && isBV opt ->
        (EPSysFun (ETBitVector True 0) (Left SFFseek) [fd, off, op], ETBitVector True 0)
      (SFFeof, [(fd, fdt)]) | isBV fdt ->
        (EPSysFun (ETBitVector True 0) (Left SFFeof) [fd], ETBitVector True 0)
      (SFFopen, [(p, t)]) | isFinBV t ->
        (EPSysFun (ETBitVector False 32) (Left SFFopen) [p], ETBitVector False 32)
      (SFFopen, [(p, pt), (t, tt)]) | isFinBV pt && isFinBV tt ->
        (EPSysFun (ETBitVector False 32) (Left SFFopen) [p, t], ETBitVector False 32)
      (SFFgetc, [(e, t)]) | isBV t ->
        (EPSysFun (ETBitVector True 0) (Left SFFgetc) [e], ETBitVector True 0)
      (SFUngetc, [(c, ct), (fd, fdt)]) | isBV ct && isBV fdt ->
        (EPSysFun (ETBitVector True 0) (Left SFUngetc) [c, fd], ETBitVector True 0)
      (SFFgets, [(s, st), (fd, fdt)] | isFinBV s && isBV fdt ->
        (EPSysFun (ETBitVector True 0) (Left SFFgets) [s, fd], ETBitVector True 0)
      (SFSscanf, [(s, st), (fmt, fmtt), (a, t)]) | isFinBV st && isFinBV fmtt && t <> ETNothing ->
        (EPSysFun (ETBitVector True 0) (Left Sscanf) [s, fmt, a], ETBitVector True 0)
      (SFRewind, [(fd, fdt)]) | isBV fdt ->
        (EPSysFun (ETBitVector True 0) (Left SFRewind) [fd], ETBitVector True 0)
      (SFFtell, [(e, t)]) | isBV t ->
        (EPSysFun (ETBitVector True 0) (Left SFFtell) [e], ETBitVector True 0)
      (SFFerror, [(fd, fdt), (s, st)]) | isBV fdt && isFinBV st ->
        (EPSysFun (ETBitVector True 0) (Left SFFerror) [fd, s], ETBitVector True 0)
      (SFRealtime, []) -> (EPSysFun ETReal (Left SFRealtime) [], ETReal)
      -- LRM 5.1.6 says time is unsigned so I assume $time also is
      (SFTime, []) -> (EPSysFun (ETBitVector False 64) (Left SFTime) [], ETBitVector True 64)
      (SFStime, []) -> (EPSysFun (ETBitVector False 32) (Left SFStime) [], ETBitVector False 32)
      (SFBitstoreal, [(e, t)]) | isBV t -> (EPSysFun ETReal (Left SFBittoreal) [e], ETReal)
      (SFItor, [(e, t)]) | isBV t -> (EPSysFun ETReal (Left SFItor) [e], ETReal)
      (SFSigned, [(e, ETUnknown)]) -> (EPSysFun ETUnknown (Left SFSigned) [e], ETUnknown)
      (SFSigned, [(e, ETBitVector _ sz)]) ->
        (EPSysFun (ETBitVector True sz) (Left SFSigned) [e], ETBitVector True sz)
      -- Signedness is not specified in the standard, assuming unsigned
      (SFRealtobits, [(e, t)]) | isReal t ->
        (EPSysFun (ETBitVector False 64) (Left SFRealtobits) [e], ETBitVector False 64)
      (SFRtoi, [(e, t)]) | isReal t ->
        (EPSysFun (ETBiteVector True 0) (Left SFRtoi) [e], ETBiteVector True 0)
      (SFUnsigned, [(e, ETUnknown)]) -> (EPSysFun ETUnknown (Left SFUnsigned) [e], ETUnknown)
      (SFUnsigned, [(e, ETBitVector _ sz)]) ->
        (EPSysFun (ETBitVector False sz) (Left SFUnsigned) [ne], ETBitVector False sz)
      (SFRandom, []) -> (EPSysFun (ETBitVector True 32) (Left SFRandom) [], ETBitVector True 32)
      (SFRandom, [(e, t)]) | isBV t ->
        (EPSysFun (ETBitVector True 32) (Left SFRandom) [e], ETBitVector True 32)
      -- all dist, function return a long, which is not necessarily 32 bits
      (SFDisterlang, [(s, st), (k, kt), (m, mt)]) | isInt st && isBV kt && isBV mt ->
        (EPSysFun (ETBitVector True 0) (Left SFDiserlang) [s, k, m], ETBitVector True 0)
      (SFDistnormal, [(se, set), (m, mt), (std, stdt)]) | isInt set && isBV mt && isBV stdt ->
        (EPSysFun (ETBitVector True 0) (Left SFDistnormal) [se, m, std], ETBitVector True 0)
      (SFDistt, [(s, st), (dof, doft)]) | isInt st && isBV doft ->
        (EPSysFun (ETBitVector True 0) (Left SFDistt) [s, dof], ETBitVector True 0)
      (SFDistchisquare, [(s, st), (dof, doft)]) | isInt st && isBV doft ->
        (EPSysFun (ETBitVector True 0) (Left SFDistchisquare) [s, dof], ETBitVector True 0)
      (SFDistexponential, [(s, st), (m, mt)]) | isInt st && isBV mt ->
        (EPSysFun (ETBitVector True 0) (Left SFDistexponential) [s, m], ETBitVector True 0)
      (SFDistpoisson, [(s, st), (m, mt)]) | isInt st && isBV mt ->
        (EPSysFun (ETBitVector True 0) (Left SFDistpoisson) [s, m], ETBitVector True 0)
      (SFDistuniform, [(se, set), (st, stt), (e, et)]) | isInt set && isBV stt && isBV et ->
        (EPSysFun (ETBitVector True 0) (Left SFDistuniform) [se, st, e], ETBitVector True 0)
      (SFClog2, [(e, t)]) | isBV t ->
        (EPSysFun (ETBitVector True 0) (Left SFClog2) [e], ETBitVector True 0)
      (SFLn, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFLn) [e], ETReal)
      (SFLog10, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFLog10) [e], ETReal)
      (SFExp, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFExp) [e], ETReal)
      (SFSqrt, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFSqrt) [e], ETReal)
      (SFPow, [(x, xt), (y, yt)]) | isReal xt && isReal yt ->
        (EPSysFun ETReal (Left SFPow) [x, y], ETReal)
      (SFFloor, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFFloor) [e], ETReal)
      (SFCeil, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFCeil) [e], ETReal)
      (SFSin, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFSin) [e], ETReal)
      (SFCos, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFCos) [e], ETReal)
      (SFTan, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFTan) [e], ETReal)
      (SFAsin, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFAsin) [e], ETReal)
      (SFAcos, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFAcos) [e], ETReal)
      (SFAtan, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFAtan) [e], ETReal)
      (SFAtan2, [(x, xt), (y, yt)]) | isReal xt && isReal yt ->
        (EPSysFun ETReal (Left SFAtan2) [x, y], ETReal)
      (SFHypot, [(x, xt), (y, yt)]) | isReal xt && isReal yt ->
        (EPSysFun ETReal (Left SFHypot) [x, y], ETReal)
      (SFSinh, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFSinh) [e], ETReal)
      (SFCosh, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFCosh) [e], ETReal)
      (SFTanh, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFTanh) [e], ETReal)
      (SFAsinh, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFAsinh) [e], ETReal)
      (SFAcosh, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFAcosh) [e], ETReal)
      (SFAtanh, [(e, t)]) | isReal t -> (EPSysFun ETReal (Left SFAtanh) [e], ETReal)
      (SFTestplusargs, [(s, t)]) | isFinBV t -> -- No result type specified, assuming integer
        (EPSysFun (ETBitVector True 0) (Left SFTestplusargs) [s], ETBitVector True 0)
      -- No result type specified, assuming integer
      (SFValueplusargs, [(s, st), (v, vt)]) | isFinBV st && isFinBV vt ->
        (EPSysFun (ETBitVector True 0) (Left SFValueplusargs) [s, v], ETBitVector True 0)
      _ -> sferr

-- | (Partially-)elaborates an expression assuming a self-determined context
elabExpr1 :: (i -> ei) -> (r -> er) -> Bool -> Expr i r a -> (ElabExpr ei er a, ElabType)
elabExpr1 fi fr partial x = case x of
  ExprPrim p -> EEPrim <$> mkP p
  ExprUnOp o a p -> case o of
    UnPlus -> EEUnOp UnPlus a <$> mkPNN p
    UnMinus -> EEUnOp UnMinus a <$> mkPNN p
    UnLNot -> let (pp, t) = mkSP p in
      if t <> ETNothing then (EEUnOp UnLNot a pp, ETBitVector False 1) else unerr
    UnNot -> let (pp, t) = mkP p in if isBV t then (EEUnOp UnNot a pp, t) else unerr
    UnAnd -> (EEUnOp UnAnd a $ mkSPNR p, ETBitVector False 1)
    UnNand -> (EEUnOp UnNand a $ mkSPNR p, ETBitVector False 1)
    UnOr -> (EEUnOp UnOr a $ mkSPNR p, ETBitVector False 1)
    UnNor -> (EEUnOp UnNor a $ mkSPNR p, ETBitVector False 1)
    UnXor -> (EEUnOp UnXor a $ mkSPNR p, ETBitVector False 1)
    UnXNor -> (EEUnOp UnXNor a $ mkSPNR p, ETBitVector False 1)
  ExprBinOp l o a r -> case o of
    BinPlus -> let (nl, nr, t) = mk2NN partial l r in (EEBinOp nl (EBPlus t) a nr, t)
    BinMinus -> let (nl, nr, t) = mk2NN partial l r in (EEBinOp nl (EBMinus t) a nr, t)
    BinTimes -> let (nl, nr, t) = mk2NN partial l r in (EEBinOp nl (EBTimes t) a nr, t)
    BinDiv -> let (nl, nr, t) = mk2NN partial l r in (EEBinOp nl (EBDiv t) a nr, t)
    BinMod -> let (nl, nr, t) = mk2NN partial l r in (EEBinOp nl (EBMod t) a nr, t)
    BinEq -> let (nl, nr, t) = mk2NN False l r in (EEBinOp nl (EBEq t) a nr, ETBitVector False 1)
    BinNEq -> let (nl, nr, t) = mk2NN False l r in (EEBinOp nl (EBNeq t) a nr, ETBitVector False 1)
    BinCEq -> let (nl, nr, t) = mk2NN False l r in (EEBinOp nl (EBCEq t) a nr, ETBitVector False 1)
    BinCNEq -> let (nl, nr, t) = mk2NN False l r in (EEBinOp nl (EBCNEq t) a nr, ETBitVector False 1)
    BinLAnd -> (EEBinOp (mkSENN l) EBLAnd a (mkSENN r), ETBitVector False 1)
    BinLOr -> (EEBinOp (mkSENN l) EBLOr a (mkSENN r), ETBitVector False 1)
    BinLT -> let (nl, nr, t) = mk2NN False l r in (EEBinOp nl (EBLT t) a nr, ETBitVector False 1)
    BinLEq -> let (nl, nr, t) = mk2NN False l r in (EEBinOp nl (EBLEq t) a nr, ETBitVector False 1)
    BinGT -> let (nl, nr, t) = mk2NN False l r in (EEBinOp nl (EBGT t) a nr, ETBitVector False 1)
    BinGEq -> let (nl, nr, t) = mk2NN False l r in (EEBinOp nl (EBGEq t) a nr, ETBitVector False 1)
    BinAnd -> let (nl, nr, t) = mk2NR l r in (EEBinOp nl (EBAnd t) a nr, t)
    BinOr -> let (nl, nr, t) = mk2NR l r in (EEBinOp nl (EBOr t) a nr, t)
    BinXor -> let (nl, nr, t) = mk2NR l r in (EEBinOp nl (EBXor t) a nr, t)
    BinXNor -> let (nl, nr, t) = mk2NR l r in (EEBinOp nl (EBXNor t) a nr, t)
    -- I assume l is context-determined because there is nothing saying otherwise
    BinPower ->
      let (nl, lt) = mk1 l
          (nr, rt) = mkSE r
       in case () of
          () | lt == ETReal || rt == ETReal ->
            (EEBinOp (mk2 partial ETReal nl) (EBPower ETReal) a nr, ETReal)
          () | lt <> ETNothing && rt <> ETNothing ->
            (EEBinOp (mk2 partial lt nl) (EBPower lt) a nr, lt)
          _ -> binerr
    BinLSL -> (\x -> EEBinOp x EBLSL a $ mkSENR r) <$> mkENR l
    BinLSR -> (\x -> EEBinOp x EBLSR a $ mkSENR r) <$> mkENR l
    BinASL -> (\x -> EEBinOp x EBASL a $ mkSENR r) <$> mkENR l
    BinASR -> (\x -> EEBinOp x EBASR a $ mkSENR r) <$> mkENR l
  ExprCond c a t f -> let (nt, nf, mt) = mk2NN partial t f in (EECond (mkSENN c) a nt nf, mt)
  where
    unerr = error "Invalid argument to unary operation"
    mkP = elabPrim1 fi fr partial
    mkSP = elabPrimSelfDet fi fr
    mkSPNR = let (np, t) = mkSP p in if isBV t then np else unerr
    mkPNN p = let (np, t) = mkP p in if t <> ETNothing then (np, t) else unerr
    mk1 = elabExpr1 fi fr True
    mk2 b = if b then const id else elabExprCtxDet2
    mkSE = elabExprSelfDet fi fr
    nerr = error "Unexpected empty concatenation"
    mkSENN e = let (ne, t) = mkSE e in if t <> ETNothing then ne else nerr
    binerr = error "Invalid argument to binary operation"
    mkENR e = let (ne, t) = elabExpr1 fi f partial e in if isBV t then (ne, t) else binerr
    mkSENR e = let (ne, t) = mkSE e in if isBV t then ne else binerr
    mk2NN b l r =
      let (nl, lt) = mk1 l
          (nr, rt) = mk1 r
          t = elabMaxType lt rt
       in if t <> ETNothing then (mk2 b t nl, mk2 b t nr, t) else nerr
    mk2NR l r =
      let (nl, lt) = mk1 l
          (nr, rt) = mk1 r
          t = elabMaxType lt rt
       in if isBV t && isBV rt then (mk2 b t nl, mk2 b t nr, t) else binerr

-- | Finishes elaboration of a primary given a context type
-- https://accellera.mantishub.io/view.php?id=2128 and LRM suggest that signedness is propagated down
elabPrimCtxDet2 :: ElabType -> ElabPrim i r a -> ElabPrim i r a
elabPrimCtxDet2 t x = case x of
  EPMinTypMax _ mtm -> EPMinTypMax t (elabExprCtxDet2 t <$> mtm)
  EPSysFun _ i a -> case i of
    Left SFSigned -> EPSysFun t (Left SFSigned) (elabSignBarrier <$> a)
    Left SFUnsigned -> EPSysFun t (Left SFUnsigned) (elabSignBarrier <$> a)
    _ -> x
  _ -> x
  where elabSignBarrier e = elabExprCtxDet2 (elabResize (elabExprGetType e) t) e

-- | Finishes elaboration of an expression given a context type
elabExprCtxDet2 :: ElabType -> ElabExpr i r a -> ElabExpr i r a
elabExprCtxDet2 t x = case x of
  EEPrim p -> EEPrim $ mkP p
  EEUnOp o a p -> case o of
    UnPlus -> EEUnOp UnPlus a $ mkP p
    UnMinus -> EEUnOp UnMinus a $ mkP p
    UnNot -> EEUnOp UnNot a $ mkP p
    _ -> x
  EEBinOp l o a r -> case o of
    EBPlus _ -> EEBinOp (mkE l) (EBPlus t) a (mkE r)
    EBMinus _ -> EEBinOp (mkE l) (EBMinus t) a (mkE r)
    EBTimes _ -> EEBinOp (mkE l) (EBTimes t) a (mkE r)
    EBDiv _ -> EEBinOp (mkE l) (EBDiv t) a (mkE r)
    EBMod _ -> EEBinOp (mkE l) (EBMod t) a (mkE r)
    EBAnd _ -> EEBinOp (mkE l) (EBAnd t) a (mkE r)
    EBOr _ -> EEBinOp (mkE l) (EBOr t) a (mkE r)
    EBXor _ -> EEBinOp (mkE l) (EBXor t) a (mkE r)
    EBXNor _ -> EEBinOp (mkE l) (EBXNor t) a (mkE r)
    EBPower _ -> EEBinOp (mkE l) (EBPower t) a r
    EBLSL -> EEBinOp (mkE l) EBLSL a r
    EBLSR -> EEBinOp (mkE l) EBLSR a r
    EBASL -> EEBinOp (mkE l) EBASL a r
    EBASR -> EEBinOp (mkE l) EBASR a r
    _ -> x
  EECond ot c a tb fb -> EECond (elabResize ot t) c a (mkE tb) (mkE fb)
  where
    mkP = elabPrimCtxDet2 t
    mkE = elabExprCtxDet2 t

-- | Elaborates a self-determined primary
elabPrimSelfDet :: (i -> ei) -> (r -> er) -> Prim i r a -> ElabPrim ei er a
elabPrimSelfDet fi fr = elabPrim1 fi fr False

-- | Elaborates a primary with a given context type
elabPrimCtxDet :: (i -> ei) -> (r -> er) -> ElabType -> Prim i r a -> ElabPrim ei er a
elabPrimCtxDet fi fr t p =
  let (np, pt) = elabPrim1 fi fr True p in elabPrimCtxDet2 (elabMaxType t pt) np

-- | Elaborates a self-determined expression
elabExprSelfDet :: (i -> ei) -> (r -> er) -> Expr i r a -> ElabExpr ei er a
elabExprSelfDet fi fr = elabExpr1 fi fr False

-- | Elaborates an expression with a given context type
elabExprCtxDet :: (i -> ei) -> (r -> er) -> ElabType -> Expr i r a -> ElabExpr ei er a
elabExprCtxDet fi fr t e =
  let (ne, et) = elabExpr1 fi fr True e in elabExprCtxDet2 (elabMaxType t et) ne

data IdentifiedElement
  = Module [ModuleBlock]
  | Primitive [PrimitiveBlock]
  | ModuleArray [(Range2 ElabCExpr, ModuleBlock)]
  | PrimitiveArray [(Range2 ElabCExpr, PrimitiveBlock)]
  | Gate [Gate Identity]
  | GenerateBlock [[Attributed ModGenBlockedItem]]
  | Function
    [ ( Bool,
        Maybe (ComType () ElabCExpr),
        [AttrIded (TFBlockDecl () ElabCExpr)],
        FunctionStatement
      )
    ]
  | Task [(Bool, [AttrIded (TFBlockDecl Dir ElabCExpr)], MybStmt)]
  | TaskPort [(Dir, ComType Bool ElabCExpr)]
  | FunPort [ComType Bool ElabCExpr]
  | StatementBlock [([AttrIded StdBlockDecl], Bool, [AttrStmt])]
  | FStatementBlock [([AttrIded StdBlockDecl], Bool, [AttrFStmt])]
  | Parameter [Parameter ElabCExpr]
  | Specparam [CMinTypMax]
  | GenVar
  | Net [(NetType, NetProp)]
  | TriReg [NetProp]
  | Variable [StdBlockDecl]
