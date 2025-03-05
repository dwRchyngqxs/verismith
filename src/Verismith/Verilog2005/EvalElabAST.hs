-- Module      : Verismith.Verilog2005.EvalElabAST
-- Description : Verilog 2005 Elaboration AST.
-- Copyright   : (c) 2024 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Verismith.Verilog2005.EvalElabAST
  ( Value (..),
    Type (..),
    ElabBinaryOperator (..),
    ElabPrim (..),
    ElabExpr (..),
    ElabGenCaseItem (..),
    ElabModGenItem (..),
    ElabModuleItem (..),
    ElabGenerateBlock (..),
    ElabModuleBlock (..),
    ElabVerilog2005 (..),
    evalBinary,
    evalBinaryUnlimited,
    evalOctal,
    evalOctalUnlimited,
    evalHexadecimal,
    evalHexadecimalUnlimited,
    evalNumber,
    evalString,
    elabExprCtxDet,
    elabNExprCtxDet,
    elabCExprCtxDet,
    elabExprSelfDet,
    elabNExprSelfDet,
    elabCExprSelfDet,
    evalTruth,
    evalExpr,
    evalNExprCtxDet,
    evalCExprCtxDet,
    evalNExprSelfDet,
    evalCExprSelfDet,
  )
where

import GHC.Generics (Generic)
import qualified Numeric.Extras as NExt
import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.ByteString.Internal (c2w, w2c, unpackChars)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import Data.Data
import Data.Bifunctor
import Data.Bits
import Data.Data.Lens
import Data.List (foldl')
import Numeric.Natural
import GHC.Float
import Verismith.Utils (nTimes, uncurry3)
import Verismith.Verilog2005.Utils (fitSz, fitSnSz)
import Verismith.Verilog2005.AST

data Value
  = VInt
    { _viSign :: !Bool,
      _viSize :: !Natural,
      _viValue :: !Integer, -- can be approximate except for concat, shifts, and padding
      -- if a bit is set, then the corresponding bit in value is one for x and zero for z
      _viXZMask :: !Integer -- cannot be approximate
    }
  | VReal !Double
  | VNothing
  deriving (Eq)

instance Show Value where
  show x = case x of
    VInt sn sz v m ->
      show
        ( PrimNumber
            sz
            sn
            (NBinary $ case red v m [] of [] -> [BXZ0]; h : t -> h :| t)
            :: Prim () () ()
        )
    VReal r -> show r
    VNothing -> "{}"
    where
      decodeBit x y = case (testBit x 0, testBit y 0) of
        (False, False) -> BXZ0
        (True, False) -> BXZ1
        (False, True) -> BXZZ
        (True, True) -> BXZX
      red x y acc =
         if x == 0 && y == 0 then acc else red (x .<<. 1) (y .<<. 1) . (: acc) $ decodeBit x y

-- | Expression types
data Type
  = TUnknown
  | TNothing
  | TReal
  | TBitVector
    { _tbvSign :: !Bool,
      _tbvSize :: !Natural -- 0 means infinite
    }
  deriving (Eq, Data, Generic)

instance Show Type where
  show x = case x of
    TUnknown -> "Unknown"
    TNothing -> "{0{}}"
    TReal -> "Real"
    TBitVector sn sz ->
      (if sn then "" else "un")
        ++ "signed "
        ++ (if sz == 0 then "integer" else "[" ++ show sz ++ "]")

isBV :: Type -> Bool
isBV t = case t of TUnknown -> True; TBitVector _ _ -> True; _ -> False

isFinBV :: Type -> Bool
isFinBV t = case t of TUnknown -> True; TBitVector _ sz -> sz /= 0; _ -> False

isInt :: Type -> Bool
isInt t = case t of TUnknown -> True; TBitVector True 0 -> True; _ -> False

isReal :: Type -> Bool
isReal t = case t of TUnknown -> True; TReal -> True; _ -> False

-- | Unary operators
data ElabUnaryOperator
  = EUPlus
  | EUMinus
  | EULNot !Type
  | EUNot
  | EUAnd !Type
  | EUNand !Type
  | EUOr !Type
  | EUNor !Type
  | EUXor !Type
  | EUXNor !Type
  deriving (Data, Generic)

instance Show ElabUnaryOperator where
  show x = case x of
    EUPlus -> "+"
    EUMinus -> "-"
    EULNot t -> "! /* " ++ show t ++ " */"
    EUNot -> "~"
    EUAnd t -> "& /* " ++ show t ++ " */"
    EUNand t -> "~& /* " ++ show t ++ " */"
    EUOr t -> "| /* " ++ show t ++ " */"
    EUNor t -> "~! /* " ++ show t ++ " */"
    EUXor t -> "^ /* " ++ show t ++ " */"
    EUXNor t -> "~^ /* " ++ show t ++ " */"

-- | Binary operators
data ElabBinaryOperator
  = EBPlus
  | EBMinus
  | EBTimes
  | EBDiv
  | EBMod
  | EBEq !Type
  | EBNEq !Type
  | EBCEq !Type
  | EBCNEq !Type
  | EBLAnd !Type
  | EBLOr !Type
  | EBLT !Type
  | EBLEq !Type
  | EBGT !Type
  | EBGEq !Type
  | EBAnd
  | EBOr
  | EBXor
  | EBXNor
  | EBPower
  | EBLSL
  | EBLSR
  | EBASL
  | EBASR
  deriving (Data, Generic)

instance Show ElabBinaryOperator where
  show x = case x of
    EBPlus -> "+"
    EBMinus -> "-"
    EBTimes -> "*"
    EBDiv -> "/"
    EBMod -> "%"
    EBEq t -> "== /* " ++ show t ++ " */"
    EBNEq t -> "!= /* " ++ show t ++ " */"
    EBCEq t -> "=== /* " ++ show t ++ " */"
    EBCNEq t -> "!== /* " ++ show t ++ " */"
    EBLAnd t -> "&& /* " ++ show t ++ " */"
    EBLOr t -> "|| /* " ++ show t ++ " */"
    EBLT t -> "< /* " ++ show t ++ " */"
    EBLEq t -> "<= /* " ++ show t ++ " */"
    EBGT t -> "> /* " ++ show t ++ " */"
    EBGEq t -> ">= /* " ++ show t ++ " */"
    EBAnd -> "&"
    EBOr -> "|"
    EBXor -> "^"
    EBXNor -> "~^"
    EBPower -> "**"
    EBLSL -> "<<"
    EBLSR -> ">>"
    EBASL -> "<<<"
    EBASR -> ">>>"

-- TODO: Prettyprint?
-- | Parametric primary expression
data ElabPrim i r a
  = EPNumber
      { _epnType :: !Type,
        _epnSigned :: !Bool,
        _epnSize :: !Natural, -- 0 means unspecified
        _epnValue :: !Integer,
        _epnXZMask :: !Integer
      }
  | EPReal !Double
  | EPIdent
      { _epiType :: !Type,
        _epiIdent :: !i,
        _epiSub :: !r
      }
  | EPConcat
      { _epcType :: !Type,
        _epcArgs :: !(NonEmpty (ElabExpr i r a))
      }
  | EPMultConcat
      { _epmcType :: !Type,
        _epmcMul :: !(ElabExpr Identifier (Maybe (RangeExpr ElabCExpr ElabCExpr)) a),
        _epmcExpr :: !(NonEmpty (ElabExpr i r a))
      }
  | EPFun
      { _epfType :: !Type,
        _epfIdent :: !i,
        _epfAttr :: !a,
        _epfArgs :: ![ElabExpr i r a]
      }
  | EPSysFun
      { _epsfType :: !Type,
        -- TODO: Maybe have the arguments fixed instead of a list
        _epsfIdent :: !(Either SystemFunction ByteString),
        _epsfArgs :: ![ElabExpr i r a]
      }
  | EPMinTypMax !(MinTypMax (ElabExpr i r a))
  deriving (Data, Generic)

-- TODO: Prettyprint?
-- | Parametric expression
data ElabExpr i r a
  = EEPrim !(ElabPrim i r a)
  | EEUnOp
      { _eeuOp :: !ElabUnaryOperator,
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
      { _eecCond :: !(ElabExpr i r a),
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

-- | Bitmask of ones
onesbm :: Natural -> Integer
onesbm sz = if sz == 0 then -1 else bit (fromEnum sz) - 1

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
            Just code | isOctalDigit c -> let ncode = fromEnum (c - c2w '0') : code in
              if length code == 2
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
elabMaxType :: Type -> Type -> Type
elabMaxType t1 t2 = case (t1, t2) of
  (TNothing, TNothing) -> TNothing
  (TNothing, _) -> err
  (_, TNothing) -> err
  (TReal, _) -> TReal
  (_, TReal) -> TReal
  (TUnknown, _) -> TUnknown
  (_, TUnknown) -> TUnknown
  (TBitVector sn1 sz1, TBitVector sn2 sz2) ->
    TBitVector (sn1 && sn2) (if sz1 == 0 || sz2 == 0 then 0 else max sz1 sz2)
  where err = error "Cannot merge empty concatenation type"

-- DOC: more precision on typing rules: https://accellera.mantishub.io/view.php?id=1072
-- | (Partially-)elaborates a primary assuming a self-determined context
elabPrim1 :: (i -> ei) -> (r -> er) -> Bool -> Prim i r a -> (ElabPrim ei er a, Type)
elabPrim1 fi fr partial x = case x of
  PrimNumber sz sn n ->
    let t = TBitVector sn sz in (uncurry (EPNumber t sn sz) $ evalNumber sz sn n, t)
  PrimReal s -> (EPReal $ read $ unpackChars s, TReal)
  PrimIdent i r -> (EPIdent TUnknown (fi i) (fr r), TUnknown)
  PrimConcat l -> case mkCat l of
    (ll, Nothing) -> (EPConcat TUnknown ll, TUnknown)
    (ll, Just sz) -> let t = TBitVector False sz in (EPConcat t ll, t)
  PrimMultConcat n l ->
    let nn = elabExprSelfDet id (fmap $ bimap elabCExprSelfDet elabCExprSelfDet) n in
    case evalExpr nn of
      Nothing -> (EPMultConcat TUnknown nn $ fst $ mkCat l, TUnknown)
      Just (VInt sn sz v 0) ->
        let mn = EEPrim $ EPNumber (TBitVector False 1) sn sz v 0
            tval = fitSnSz sn sz v
         in case compare tval 0 of
            LT -> caterr
            EQ -> let t = TNothing in
              (EPMultConcat t mn [EEPrim $ EPNumber (TBitVector False 1) False 1 1 0], t)
            GT -> case mkCat l of
              (ll, Nothing) -> (EPMultConcat TUnknown mn ll, TUnknown)
              (ll, Just ssz) ->
                let t = TBitVector False $ ssz * fromIntegral tval in (EPMultConcat t mn ll, t)
      _ -> caterr
  PrimFun i attr args -> (EPFun TUnknown (fi i) attr $ fst . mkSD <$> args, TUnknown)
  PrimSysFun s args ->
    maybe (EPSysFun TUnknown (Right s) $ fst . mkSD <$> args, TUnknown) (mkSF args) $
      HashMap.lookup s sfMap
  PrimMinTypMax (MTMSingle e) -> let (ne, t) = mkE e in (EPMinTypMax $ MTMSingle ne, t)
  PrimMinTypMax (MTMFull em et eM) ->
    let (nem, tm) = mkE em
        (net, tt) = mkE et
        (neM, tM) = mkE eM
        t = elabMaxType (elabMaxType tm tt) tM
     in (EPMinTypMax $ MTMFull nem net neM, t)
  PrimString b ->
    let (sz, v) = evalString b
        t = TBitVector False sz
     in (EPNumber t False sz v 0, t)
  where
    caterr = error "Error in concatenation"
    sferr = error "Invalid arguments to system function"
    mkSD = elabExpr1 fi fr False
    mkE = elabExpr1 fi fr partial
    auxCat e (al, at) = let (x, t) = mkE e in case (t, at) of
      (TNothing, _) -> (al, at)
      (TUnknown, _) -> (x : al, Nothing)
      (_, Nothing) | isFinBV t -> (x : al, Nothing)
      (TBitVector _ nsz, Just sz) | nsz /= 0 -> (x : al, Just $ sz + nsz)
      _ -> caterr
    mkCat l = case foldr auxCat ([], Just 0) l of
      (h : t, Nothing) -> (h :| t, Nothing)
      (h : t, Just sz) | sz /= 0 -> (h :| t, Just sz)
      _ -> caterr
    mkSF l sf = case (sf, mkSD <$> l) of -- integer is considered signed with unlimited bits
      (SFFscanf, [(fd, fdt), (fmt, fmtt), (a, t)]) | isBV fdt && isFinBV fmtt && t /= TNothing ->
        (EPSysFun (TBitVector True 0) (Left SFFscanf) [fd, fmt, a], TBitVector True 0)
      (SFFread, [(m, mt), (fd, fdt)]) | isFinBV mt && isBV fdt ->
        (EPSysFun (TBitVector True 0) (Left SFFread) [m, fd], TBitVector True 0)
      (SFFread, [(m, mt), (fd, fdt), (s, st)]) | isFinBV mt && isBV fdt && isBV st ->
        (EPSysFun (TBitVector True 0) (Left SFFread) [m, fd, s], TBitVector True 0)
      (SFFread, [(m, mt), (fd, fdt), (s, st), (c, ct)])
        | isFinBV mt && isBV fdt && isBV st && isBV ct ->
        (EPSysFun (TBitVector True 0) (Left SFFread) [m, fd, s, c], TBitVector True 0)
      (SFFseek, [(fd, fdt), (off, offt), (op, opt)]) | isBV fdt && isBV offt && isBV opt ->
        (EPSysFun (TBitVector True 0) (Left SFFseek) [fd, off, op], TBitVector True 0)
      (SFFeof, [(fd, fdt)]) | isBV fdt ->
        (EPSysFun (TBitVector True 0) (Left SFFeof) [fd], TBitVector True 0)
      (SFFopen, [(p, t)]) | isFinBV t ->
        (EPSysFun (TBitVector False 32) (Left SFFopen) [p], TBitVector False 32)
      (SFFopen, [(p, pt), (t, tt)]) | isFinBV pt && isFinBV tt ->
        (EPSysFun (TBitVector False 32) (Left SFFopen) [p, t], TBitVector False 32)
      (SFFgetc, [(e, t)]) | isBV t ->
        (EPSysFun (TBitVector True 0) (Left SFFgetc) [e], TBitVector True 0)
      (SFUngetc, [(c, ct), (fd, fdt)]) | isBV ct && isBV fdt ->
        (EPSysFun (TBitVector True 0) (Left SFUngetc) [c, fd], TBitVector True 0)
      (SFFgets, [(s, st), (fd, fdt)]) | isFinBV st && isBV fdt ->
        (EPSysFun (TBitVector True 0) (Left SFFgets) [s, fd], TBitVector True 0)
      (SFSscanf, [(s, st), (fmt, fmtt), (a, t)]) | isFinBV st && isFinBV fmtt && t /= TNothing ->
        (EPSysFun (TBitVector True 0) (Left SFSscanf) [s, fmt, a], TBitVector True 0)
      (SFRewind, [(fd, fdt)]) | isBV fdt ->
        (EPSysFun (TBitVector True 0) (Left SFRewind) [fd], TBitVector True 0)
      (SFFtell, [(e, t)]) | isBV t ->
        (EPSysFun (TBitVector True 0) (Left SFFtell) [e], TBitVector True 0)
      (SFFerror, [(fd, fdt), (s, st)]) | isBV fdt && isFinBV st ->
        (EPSysFun (TBitVector True 0) (Left SFFerror) [fd, s], TBitVector True 0)
      (SFRealtime, []) -> (EPSysFun TReal (Left SFRealtime) [], TReal)
      -- LRM 5.1.6 says time is unsigned so I assume $time also is
      (SFTime, []) -> (EPSysFun (TBitVector False 64) (Left SFTime) [], TBitVector True 64)
      (SFStime, []) -> (EPSysFun (TBitVector False 32) (Left SFStime) [], TBitVector False 32)
      (SFBitstoreal, [(e, t)]) | isBV t -> (EPSysFun TReal (Left SFBitstoreal) [e], TReal)
      (SFItor, [(e, t)]) | isBV t -> (EPSysFun TReal (Left SFItor) [e], TReal)
      (SFSigned, [(e, TUnknown)]) -> (EPSysFun TUnknown (Left SFSigned) [e], TUnknown)
      (SFSigned, [(e, TBitVector _ sz)]) ->
        (EPSysFun (TBitVector True sz) (Left SFSigned) [e], TBitVector True sz)
      -- Signedness is not specified in the standard, assuming unsigned
      (SFRealtobits, [(e, t)]) | isReal t ->
        (EPSysFun (TBitVector False 64) (Left SFRealtobits) [e], TBitVector False 64)
      (SFRtoi, [(e, t)]) | isReal t ->
        (EPSysFun (TBitVector True 0) (Left SFRtoi) [e], TBitVector True 0)
      (SFUnsigned, [(e, TUnknown)]) -> (EPSysFun TUnknown (Left SFUnsigned) [e], TUnknown)
      (SFUnsigned, [(e, TBitVector _ sz)]) ->
        (EPSysFun (TBitVector False sz) (Left SFUnsigned) [e], TBitVector False sz)
      (SFRandom, []) -> (EPSysFun (TBitVector True 32) (Left SFRandom) [], TBitVector True 32)
      (SFRandom, [(e, t)]) | isBV t ->
        (EPSysFun (TBitVector True 32) (Left SFRandom) [e], TBitVector True 32)
      -- all dist, function return a long, which is not necessarily 32 bits
      (SFDisterlang, [(s, st), (k, kt), (m, mt)]) | isInt st && isBV kt && isBV mt ->
        (EPSysFun (TBitVector True 0) (Left SFDisterlang) [s, k, m], TBitVector True 0)
      (SFDistnormal, [(se, set), (m, mt), (std, stdt)]) | isInt set && isBV mt && isBV stdt ->
        (EPSysFun (TBitVector True 0) (Left SFDistnormal) [se, m, std], TBitVector True 0)
      (SFDistt, [(s, st), (dof, doft)]) | isInt st && isBV doft ->
        (EPSysFun (TBitVector True 0) (Left SFDistt) [s, dof], TBitVector True 0)
      (SFDistchisquare, [(s, st), (dof, doft)]) | isInt st && isBV doft ->
        (EPSysFun (TBitVector True 0) (Left SFDistchisquare) [s, dof], TBitVector True 0)
      (SFDistexponential, [(s, st), (m, mt)]) | isInt st && isBV mt ->
        (EPSysFun (TBitVector True 0) (Left SFDistexponential) [s, m], TBitVector True 0)
      (SFDistpoisson, [(s, st), (m, mt)]) | isInt st && isBV mt ->
        (EPSysFun (TBitVector True 0) (Left SFDistpoisson) [s, m], TBitVector True 0)
      (SFDistuniform, [(se, set), (st, stt), (e, et)]) | isInt set && isBV stt && isBV et ->
        (EPSysFun (TBitVector True 0) (Left SFDistuniform) [se, st, e], TBitVector True 0)
      (SFClog2, [(e, t)]) | isBV t ->
        (EPSysFun (TBitVector True 0) (Left SFClog2) [e], TBitVector True 0)
      (SFLn, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFLn) [e], TReal)
      (SFLog10, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFLog10) [e], TReal)
      (SFExp, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFExp) [e], TReal)
      (SFSqrt, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFSqrt) [e], TReal)
      (SFPow, [(x, xt), (y, yt)]) | isReal xt && isReal yt ->
        (EPSysFun TReal (Left SFPow) [x, y], TReal)
      (SFFloor, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFFloor) [e], TReal)
      (SFCeil, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFCeil) [e], TReal)
      (SFSin, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFSin) [e], TReal)
      (SFCos, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFCos) [e], TReal)
      (SFTan, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFTan) [e], TReal)
      (SFAsin, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFAsin) [e], TReal)
      (SFAcos, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFAcos) [e], TReal)
      (SFAtan, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFAtan) [e], TReal)
      (SFAtan2, [(x, xt), (y, yt)]) | isReal xt && isReal yt ->
        (EPSysFun TReal (Left SFAtan2) [x, y], TReal)
      (SFHypot, [(x, xt), (y, yt)]) | isReal xt && isReal yt ->
        (EPSysFun TReal (Left SFHypot) [x, y], TReal)
      (SFSinh, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFSinh) [e], TReal)
      (SFCosh, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFCosh) [e], TReal)
      (SFTanh, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFTanh) [e], TReal)
      (SFAsinh, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFAsinh) [e], TReal)
      (SFAcosh, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFAcosh) [e], TReal)
      (SFAtanh, [(e, t)]) | isReal t -> (EPSysFun TReal (Left SFAtanh) [e], TReal)
      (SFTestplusargs, [(s, t)]) | isFinBV t -> -- No result type specified, assuming integer
        (EPSysFun (TBitVector True 0) (Left SFTestplusargs) [s], TBitVector True 0)
      -- No result type specified, assuming integer
      (SFValueplusargs, [(s, st), (v, vt)]) | isFinBV st && isFinBV vt ->
        (EPSysFun (TBitVector True 0) (Left SFValueplusargs) [s, v], TBitVector True 0)
      _ -> sferr

-- | (Partially-)elaborates an expression assuming a self-determined context
elabExpr1 :: (i -> ei) -> (r -> er) -> Bool -> Expr i r a -> (ElabExpr ei er a, Type)
elabExpr1 fi fr partial x = case x of
  ExprPrim p -> first EEPrim $ mkP p
  ExprUnOp o a p -> case o of
    UnPlus -> first (EEUnOp EUPlus a) $ mkPNN p
    UnMinus -> first (EEUnOp EUMinus a) $ mkPNN p
    UnLNot ->
      let (pp, pt) = mkSP p
          t = TBitVector False 1
       in if pt /= TNothing then (EEUnOp (EULNot t) a pp, t) else unerr
    UnNot -> let (pp, t) = mkP p in if isBV t then (EEUnOp EUNot a pp, t) else unerr
    UnAnd -> mkSPNR EUAnd a p
    UnNand -> mkSPNR EUNand a p
    UnOr -> mkSPNR EUOr a p
    UnNor -> mkSPNR EUNor a p
    UnXor -> mkSPNR EUXor a p
    UnXNor -> mkSPNR EUXNor a p
  ExprBinOp l o a r -> case o of
    BinPlus -> let (nl, nr, t) = mk2NN l r in (EEBinOp nl EBPlus a nr, t)
    BinMinus -> let (nl, nr, t) = mk2NN l r in (EEBinOp nl EBMinus a nr, t)
    BinTimes -> let (nl, nr, t) = mk2NN l r in (EEBinOp nl EBTimes a nr, t)
    BinDiv -> let (nl, nr, t) = mk2NN l r in (EEBinOp nl EBDiv a nr, t)
    BinMod -> let (nl, nr, t) = mk2NN l r in (EEBinOp nl EBMod a nr, t)
    BinEq -> mk2SNN l EBEq a r
    BinNEq -> mk2SNN l EBNEq a r
    BinCEq ->
      let (nl, nr, _) = mk2NR False l r
          t = TBitVector False 1
       in (EEBinOp nl (EBCEq t) a nr, t)
    BinCNEq ->
      let (nl, nr, _) = mk2NR False l r
          t = TBitVector False 1
       in (EEBinOp nl (EBCNEq t) a nr, t)
    BinLAnd -> let t = TBitVector False 1 in (EEBinOp (mkSENN l) (EBLAnd t) a (mkSENN r), t)
    BinLOr -> let t = TBitVector False 1 in (EEBinOp (mkSENN l) (EBLOr t) a (mkSENN r), t)
    BinLT -> mk2SNN l EBLT a r
    BinLEq -> mk2SNN l EBLEq a r
    BinGT -> mk2SNN l EBGT a r
    BinGEq -> mk2SNN l EBGEq a r
    BinAnd -> let (nl, nr, t) = mk2NR partial l r in (EEBinOp nl EBAnd a nr, t)
    BinOr -> let (nl, nr, t) = mk2NR partial l r in (EEBinOp nl EBOr a nr, t)
    BinXor -> let (nl, nr, t) = mk2NR partial l r in (EEBinOp nl EBXor a nr, t)
    BinXNor -> let (nl, nr, t) = mk2NR partial l r in (EEBinOp nl EBXNor a nr, t)
    -- I assume l is context-determined because there is nothing saying otherwise
    BinPower ->
      let (nl, lt) = mk1 l
          (nr, rt) = mkSE r
       in case () of
          () | lt == TReal || rt == TReal ->
            (EEBinOp (mk2 partial TReal nl) EBPower a nr, TReal)
          () | lt /= TNothing && rt /= TNothing ->
            (EEBinOp (mk2 partial lt nl) EBPower a nr, lt)
          _ -> binerr
    BinLSL -> first (\x -> EEBinOp x EBLSL a $ mkSENR r) $ mkENR l
    BinLSR -> first (\x -> EEBinOp x EBLSR a $ mkSENR r) $ mkENR l
    BinASL -> first (\x -> EEBinOp x EBASL a $ mkSENR r) $ mkENR l
    BinASR -> first (\x -> EEBinOp x EBASR a $ mkSENR r) $ mkENR l
  ExprCond c a t f -> let (nt, nf, mt) = mk2NN t f in (EECond (mkSENN c) a nt nf, mt)
  where
    unerr = error "Invalid argument to unary operation"
    mkP = elabPrim1 fi fr partial
    mkSP = elabPrim1 fi fr False
    mkSPNR uo a p =
      let (np, pt) = mkSP p
          t = TBitVector False 1
       in if isBV pt then (EEUnOp (uo t) a np, t) else unerr
    mkPNN p = let (np, t) = mkP p in if t /= TNothing then (np, t) else unerr
    mk1 = elabExpr1 fi fr True
    mk2 b = if b then const id else elabExprCtxDet2
    mkSE = elabExpr1 fi fr False
    nerr = error "Unexpected empty concatenation"
    mkSENN e = let (ne, t) = mkSE e in if t /= TNothing then ne else nerr
    binerr = error "Invalid argument to binary operation"
    mkENR e = let (ne, t) = elabExpr1 fi fr partial e in if isBV t then (ne, t) else binerr
    mkSENR e = let (ne, t) = mkSE e in if isBV t then ne else binerr
    mk2NN l r =
      let (nl, lt) = mk1 l
          (nr, rt) = mk1 r
          t = elabMaxType lt rt
       in if t /= TNothing then (mk2 partial t nl, mk2 partial t nr, t) else nerr
    mk2SNN l bo a r =
      let (nl, lt) = mk1 l
          (nr, rt) = mk1 r
          mt = elabMaxType lt rt
          t = TBitVector False 1
       in if mt /= TNothing
          then (EEBinOp (elabExprCtxDet2 mt nl) (bo t) a (elabExprCtxDet2 mt nr), t)
          else nerr
    mk2NR b l r =
      let (nl, lt) = mk1 l
          (nr, rt) = mk1 r
          t = elabMaxType lt rt
       in if isBV t && isBV rt then (mk2 b t nl, mk2 b t nr, t) else binerr

-- | Finishes elaboration of a primary given a context type
-- https://accellera.mantishub.io/view.php?id=2128 and LRM suggest that signedness is propagated down
elabPrimCtxDet2 :: Type -> ElabPrim i r a -> ElabPrim i r a
elabPrimCtxDet2 t x = case x of
  EPNumber _ sn sz v m -> EPNumber t sn sz v m
  EPReal d -> EPReal d
  EPIdent _ i r -> EPIdent t i r
  EPConcat _ l -> EPConcat t l
  EPMultConcat _ n l -> EPMultConcat t n l
  EPFun _ i a l -> EPFun t i a l
  EPSysFun _ i l -> EPSysFun t i l
  EPMinTypMax mtm -> EPMinTypMax (elabExprCtxDet2 t <$> mtm)

-- | Finishes elaboration of an expression given a context type
elabExprCtxDet2 :: Type -> ElabExpr i r a -> ElabExpr i r a
elabExprCtxDet2 t x = case x of
  EEPrim p -> EEPrim $ mkP p
  EEUnOp o a p -> case o of
    EUPlus -> EEUnOp EUPlus a $ mkP p
    EUMinus -> EEUnOp EUMinus a $ mkP p
    EULNot _ -> EEUnOp (EULNot t) a p
    EUNot -> EEUnOp EUNot a $ mkP p
    EUAnd _ -> EEUnOp (EUAnd t) a p
    EUNand _ -> EEUnOp (EUNand t) a p
    EUOr _ -> EEUnOp (EUOr t) a p
    EUNor _ -> EEUnOp (EUNor t) a p
    EUXor _ -> EEUnOp (EUXor t) a p
    EUXNor _ -> EEUnOp (EUXNor t) a p
  EEBinOp l o a r -> case o of
    EBPlus -> EEBinOp (mkE l) EBPlus a (mkE r)
    EBMinus -> EEBinOp (mkE l) EBMinus a (mkE r)
    EBTimes -> EEBinOp (mkE l) EBTimes a (mkE r)
    EBDiv -> EEBinOp (mkE l) EBDiv a (mkE r)
    EBMod -> EEBinOp (mkE l) EBMod a (mkE r)
    EBEq _ -> EEBinOp l (EBEq t) a r
    EBNEq _ -> EEBinOp l (EBNEq t) a r
    EBCEq _ -> EEBinOp l (EBCEq t) a r
    EBCNEq _ -> EEBinOp l (EBCNEq t) a r
    EBLAnd _ -> EEBinOp l (EBLAnd t) a r
    EBLOr _ -> EEBinOp l (EBLOr t) a r
    EBLT _ -> EEBinOp l (EBLT t) a r
    EBLEq _ -> EEBinOp l (EBLEq t) a r
    EBGT _ -> EEBinOp l (EBGT t) a r
    EBGEq _ -> EEBinOp l (EBGEq t) a r
    EBAnd -> EEBinOp (mkE l) EBAnd a (mkE r)
    EBOr -> EEBinOp (mkE l) EBOr a (mkE r)
    EBXor -> EEBinOp (mkE l) EBXor a (mkE r)
    EBXNor -> EEBinOp (mkE l) EBXNor a (mkE r)
    EBPower -> EEBinOp (mkE l) EBPower a r
    EBLSL -> EEBinOp (mkE l) EBLSL a r
    EBLSR -> EEBinOp (mkE l) EBLSR a r
    EBASL -> EEBinOp (mkE l) EBASL a r
    EBASR -> EEBinOp (mkE l) EBASR a r
  EECond c a tb fb -> EECond c a (mkE tb) (mkE fb)
  where
    mkP = elabPrimCtxDet2 t
    mkE = elabExprCtxDet2 t

-- | Elaborates a self-determined primary
elabPrimSelfDet :: (i -> ei) -> (r -> er) -> Prim i r a -> ElabPrim ei er a
elabPrimSelfDet fi fr = fst . elabPrim1 fi fr False

-- | Elaborates a primary with a given context type
elabPrimCtxDet :: (i -> ei) -> (r -> er) -> Type -> Prim i r a -> ElabPrim ei er a
elabPrimCtxDet fi fr t p =
  let (np, pt) = elabPrim1 fi fr True p in elabPrimCtxDet2 (elabMaxType t pt) np

-- | Elaborates a self-determined expression
elabExprSelfDet :: (i -> ei) -> (r -> er) -> Expr i r a -> ElabExpr ei er a
elabExprSelfDet fi fr = fst . elabExpr1 fi fr False

elabNExprSelfDet :: NExpr -> ElabNExpr
elabNExprSelfDet (NExpr e) =
  ElabNExpr $
    elabExprSelfDet (fmap elabCExprSelfDet) (fmap $ bimap elabNExprSelfDet elabCExprSelfDet) e

elabCExprSelfDet :: CExpr -> ElabCExpr
elabCExprSelfDet (CExpr e) =
  ElabCExpr $ elabExprSelfDet id (fmap $ bimap elabCExprSelfDet elabCExprSelfDet) e

-- | Elaborates an expression with a given context type
elabExprCtxDet :: (i -> ei) -> (r -> er) -> Type -> Expr i r a -> ElabExpr ei er a
elabExprCtxDet fi fr t e =
  let (ne, et) = elabExpr1 fi fr True e in elabExprCtxDet2 (elabMaxType t et) ne

elabNExprCtxDet :: Type -> NExpr -> ElabNExpr
elabNExprCtxDet t (NExpr e) =
  ElabNExpr $
    elabExprCtxDet (fmap elabCExprSelfDet) (fmap $ bimap elabNExprSelfDet elabCExprSelfDet) t e

elabCExprCtxDet :: Type -> CExpr -> ElabCExpr
elabCExprCtxDet t (CExpr e) =
  ElabCExpr $ elabExprCtxDet id (fmap $ bimap elabCExprSelfDet elabCExprSelfDet) t e

-- | Makes a VInt by extending the size of `v` and `m` from `osz` to `nsz`
castVInt :: Bool -> Natural -> Natural -> Integer -> Integer -> Value
castVInt sn nsz osz v m =
  uncurry (VInt sn nsz) $ if sn then (v, m) else (signExtend v, signExtend m)
  where
    iosz = fromEnum osz
    bm = ((1 .<<. fromEnum (nsz - osz)) - 1) .<<. iosz
    signExtend x = if nsz == 0 then fitSnSz sn osz x else if testBit x iosz then x .|. bm else x

-- | Casts an int to a VReal
castVReal :: Bool -> Natural -> Integer -> Integer -> Value
castVReal sn sz v m = VReal $ encodeFloat (fitSnSz sn sz (v .&. complement m)) 0

-- | Casting a value according to a bigger type
evalCast :: Type -> Value -> Maybe Value
evalCast t v = case t of
  TUnknown -> Nothing
  TNothing -> if v == VNothing then Just v else Nothing
  TReal -> case v of
    VNothing -> Nothing
    VReal _ -> Just v
    VInt sn sz v m -> Just $ castVReal sn sz v m
  TBitVector sn sz -> case v of
    VInt _ osz v m -> Just $ castVInt sn sz osz v m
    _ -> Nothing

-- | Returns truth value of an integer
evalIntTruth :: Integer -> Integer -> ZOX
evalIntTruth v m = if v .&. complement m /= 0 then ZOXO else if m == 0 then ZOXZ else ZOXX

-- | Returns truth value of a value
evalTruth :: Value -> ZOX
evalTruth x = case x of
  VReal r -> if r /= 0 then ZOXO else ZOXZ
  VInt _ _ v m -> evalIntTruth v m
  _ -> error "Cannot tell truth value"

-- | Evaluates an elaborated primary
evalPrim :: ElabPrim i r a -> Maybe Value
evalPrim x = case x of
  EPNumber t sn sz v m -> evalCast t $ VInt sn sz v m
  EPReal d -> Just $ VReal d
  EPIdent t _ _ -> Nothing
  EPConcat t l -> evalCat l >>= evalCast t . uncurry3 (VInt False)
  EPMultConcat t n l -> do
    mult <- evalExpr n
    v <- case mult of
      VInt sn sz v 0 -> let tval = fitSnSz sn sz v in case compare tval 0 of
        LT -> error "Negative replication is not allowed"
        EQ -> return VNothing
        GT -> do
          (sz, v, m) <- evalCat l
          let isz = fromEnum sz
              f x = nTimes (\y -> (y .<<. isz) .|. x) 0 tval
          return $ VInt False (sz * fromInteger tval) (f v) (f m)
      _ -> error "Invalid multiplicity for replication"
    evalCast t v
  EPFun _ _ _ _ -> Nothing
  EPSysFun t i args -> do
    l <- traverse evalExpr args
    v <- case (i, l) of
      (Left SFUnsigned, [VInt sn sz v m]) -> Just $ VInt False sz v m
      (Left SFSigned, [VInt sn sz v m]) -> Just $ VInt True sz v m
      -- inherited from old verilog so likely unsigned
      (Left SFRealtobits, [VReal r]) -> Just $ VInt False 64 (toInteger $ castDoubleToWord64 r) 0
      (Left SFBitstoreal, [VInt _ 64 v 0]) -> Just $ VReal $ castWord64ToDouble $ fromInteger v
      (Left SFRtoi, [VReal r]) | isDoubleFinite r /= 0 -> Just $ VInt True 0 (truncate r) 0
      (Left SFItor, [VInt sn sz v m]) -> Just $ castVReal sn sz v m
      (Left SFClog2, [VInt _ sz v 0]) ->
        Just $ VInt True 0 (toInteger $ locateFirstBit (fitSnSz False sz v) $ fromEnum sz) 0
      (Left SFLn, [VReal r]) -> Just $ VReal $ log r
      (Left SFLog10, [VReal r]) -> Just $ VReal $ logBase 10 r
      (Left SFExp, [VReal r]) -> Just $ VReal $ exp r
      (Left SFSqrt, [VReal r]) -> Just $ VReal $ sqrt r
      (Left SFPow, [VReal r0, VReal r1]) -> Just $ VReal $ r0 ** r1
      (Left SFFloor, [VReal r]) -> Just $ VReal $ NExt.floor r
      (Left SFCeil, [VReal r]) -> Just $ VReal $ NExt.ceil r
      (Left SFSin, [VReal r]) -> Just $ VReal $ sin r
      (Left SFCos, [VReal r]) -> Just $ VReal $ cos r
      (Left SFTan, [VReal r]) -> Just $ VReal $ tan r
      (Left SFAsin, [VReal r]) -> Just $ VReal $ asin r
      (Left SFAcos, [VReal r]) -> Just $ VReal $ acos r
      (Left SFAtan, [VReal r]) -> Just $ VReal $ atan r
      (Left SFAtan2, [VReal r0, VReal r1]) -> Just $ VReal $ atan2 r0 r1
      (Left SFHypot, [VReal r0, VReal r1]) -> Just $ VReal $ NExt.hypot r0 r1
      (Left SFSinh, [VReal r]) -> Just $ VReal $ sinh r
      (Left SFCosh, [VReal r]) -> Just $ VReal $ cosh r
      (Left SFTanh, [VReal r]) -> Just $ VReal $ tanh r
      (Left SFAsinh, [VReal r]) -> Just $ VReal $ asinh r
      (Left SFAcosh, [VReal r]) -> Just $ VReal $ acosh r
      (Left SFAtanh, [VReal r]) -> Just $ VReal $ atanh r
      (Left _, _) -> Nothing
      _ -> error "Unrecognised system function"
    evalCast t v
  EPMinTypMax (MTMSingle e) -> evalExpr e
  EPMinTypMax mtm -> Nothing
  where
    locateFirstBit bv h =
      if h == 0 then 0 else let pos = h - 1 in if testBit bv pos then pos else locateFirstBit bv pos
    caterr = error "Invalid argument in concatention"
    auxCat a x = a >>= \(sz, v, m) -> evalExpr x >>= \y -> case y of
      VNothing -> Just (sz, v, m)
      VInt _ szsz vv mm | sz /= 0 ->
        let isz = fromEnum szsz
            bm = bit isz - 1
            f x y = (x .<<. isz) .|. (y .&. bm)
         in Just (sz + szsz, f v vv, f m mm)
      _ -> caterr
    evalCat l =
      (\(sz, v, m) -> if sz /= 0 then (sz, v, m) else caterr) <$> foldl' auxCat (Just (0, 0, 0)) l

-- | Evaluates an elaborated expression
evalExpr :: ElabExpr i r a -> Maybe Value
evalExpr x = case x of
  EEPrim p -> evalPrim p
  EEUnOp o _ p -> evalPrim p >>= \v -> case (o, v) of
    (EUPlus, VInt sn sz _ m) | m /= 0 -> Just $ let bm = onesbm sz in VInt sn sz bm bm
    (EUPlus, _) -> Just v
    (EUMinus, VInt sn sz v 0) -> Just $ VInt sn sz (fitSz sz $ -v) 0
    (EUMinus, VInt sn sz _ _) -> Just $ let bm = onesbm sz in VInt sn sz bm bm
    (EUMinus, VReal v) -> Just $ VReal (-v)
    (EULNot t, _) -> mk1 t $ let b = evalTruth v in (b /= ZOXO, b == ZOXX)
    (EUNot, VInt sn sz v m) -> Just $ VInt sn sz (fitSz sz $ complement v .|. m) m
    (EUAnd t, VInt _ _ v m) ->
      mk1 t $ let no_zeros = complement (v .|. m) == 0 in (no_zeros, no_zeros && m /= 0)
    (EUNand t, VInt _ _ v m) -> let has_zeros = complement (v .|. m) /= 0 in
      mk1 t (has_zeros || m /= 0, not has_zeros && m /= 0)
    (EUOr t, VInt _ _ v m) -> mk1 t $ let b = evalIntTruth v m in (b /= ZOXZ, b == ZOXX)
    (EUNor t, VInt _ _ v m) -> mk1 t $ let b = evalIntTruth v m in (b /= ZOXO, b == ZOXX)
    (EUXor t, VInt _ _ v m) -> mk1 t (m /= 0 || testBit (popCount v) 0, m /= 0)
    (EUXNor t, VInt _ _ v m) -> mk1 t (m /= 0 || not (testBit (popCount v) 0), m /= 0)
    _ -> error "Invalid argument to unary operation"
  EEBinOp l o _ r -> case o of
    EBPlus -> binIntReal l r (\l r -> VReal $ l + r) $ binA $ \_ sz vl vr -> (fitSz sz $ vl + vr, 0)
    EBMinus ->
      binIntReal l r (\l r -> VReal $ l - r) $ binA $ \_ sz vl vr -> (fitSz sz $ vl - vr, 0)
    EBTimes ->
      binIntReal l r (\l r -> VReal $ l * r) $ binA $ \_ sz vl vr -> (fitSz sz $ vl * vr, 0)
    EBDiv -> binIntReal l r (\l r -> VReal $ l / r) $ binA $ \sn sz vl vr ->
      let ll = fitSnSz sn sz vl
          rr = fitSnSz sn sz vr
       in case () of
          () | (sz == 0 && rr /= 0) || (ll >= 0 && rr > 0) || (ll <= 0 && rr < 0) -> (div ll rr, 0)
          () | rr /= 0 -> (-div (-ll) rr, 0)
          () -> let bm = onesbm sz in (bm, bm)
    EBMod -> binIntStrict l r $ binA $ \sn sz vl vr ->
      let ll = fitSnSz sn sz vl
          rr = fitSnSz sn sz vr
       in case () of
          () | (sz == 0 && rr /= 0) || (ll >= 0 && rr > 0) || (ll <= 0 && rr < 0) -> (mod ll rr, 0)
          () | rr /= 0 -> (-mod (-ll) rr, 0)
          () -> let bm = onesbm sz in (bm, bm)
    EBEq t -> (binIntReal l r (\l r -> (l == r, False)) $ binX $ \_ _ -> (==)) >>= mk1 t
    EBNEq t -> (binIntReal l r (\l r -> (l /= r, False)) $ binX $ \_ _ -> (/=)) >>= mk1 t
    EBCEq t -> (binIntStrict l r $ \_ _ vl ml vr mr -> (vl == vr && ml == mr, False)) >>= mk1 t
    EBCNEq t -> (binIntStrict l r $ \_ _ vl ml vr mr -> (vl /= vr || ml /= mr, False)) >>= mk1 t
    EBLAnd t -> do
      ll <- evalExpr l
      v <- case evalTruth ll of
        ZOXZ -> Just (False, False)
        ZOXO -> (\rr -> let b = evalTruth rr in (b /= ZOXZ, b == ZOXX)) <$> evalExpr r
        ZOXX -> (\rr -> let b = evalTruth rr /= ZOXZ in (b, b)) <$> evalExpr r
      mk1 t v
    EBLOr t -> do
      ll <- evalExpr l
      v <- case evalTruth ll of
        ZOXZ -> (\rr -> let b = evalTruth rr in (b /= ZOXZ, b == ZOXX)) <$> evalExpr r
        ZOXO -> Just (True, False)
        ZOXX -> (\rr -> (True, evalTruth rr /= ZOXO)) <$> evalExpr r
      mk1 t v
    EBLT t -> (>>= mk1 t) $ binIntReal l r (\l r -> (l < r, False)) $ binX $ \sn sz vl vr ->
      fitSnSz sn sz vl < fitSnSz sn sz vr
    EBLEq t -> (>>= mk1 t) $ binIntReal l r (\l r -> (l <= r, False)) $ binX $ \sn sz vl vr ->
      fitSnSz sn sz vl <= fitSnSz sn sz vr
    EBGT t -> (>>= mk1 t) $ binIntReal l r (\l r -> (l > r, False)) $ binX $ \sn sz vl vr ->
      fitSnSz sn sz vl > fitSnSz sn sz vr
    EBGEq t -> (>>= mk1 t) $ binIntReal l r (\l r -> (l >= r, False)) $ binX $ \sn sz vl vr ->
      fitSnSz sn sz vl >= fitSnSz sn sz vr
    EBAnd -> binIntStrict l r $ \sn sz vl ml vr mr ->
      let nz = (vl .|. ml) .&. (vr .|. mr) in VInt sn sz nz ((ml .|. mr) .&. nz)
    EBOr -> binIntStrict l r $ \sn sz vl ml vr mr ->
      let ones = (vl .&. complement ml) .|. (vr .&. complement mr)
          npm = ml .|. mr
      in VInt sn sz (npm .|. ones) (npm .&. complement ones)
    EBXor -> binIntStrict l r $ \sn sz vl ml vr mr ->
      let nm = ml .|. mr in VInt sn sz ((vl .^. vr) .|. nm) nm
    EBXNor -> binIntStrict l r $ \sn sz vl ml vr mr ->
      let nm = ml .|. mr in VInt sn sz (complement (vl .^. vr) .|. nm) nm
    EBPower -> case (evalExpr l, evalExpr r) of
      (_, Just (VReal 0)) -> Just $ VReal 1
      (Just (VReal 1), _) -> Just $ VReal 1
      (Just (VReal 0), _) -> Just $ VReal 0
      (Just (VReal ll), Just (VReal rr)) -> Just $ VReal $ ll ** rr
      (Just (VReal _), Just (VInt _ _ _ m)) | m /= 0 -> Just $ VReal 0 -- `x` to real
      (Just (VReal _), Just (VInt _ _ 0 0)) -> Just $ VReal 1
      (Just (VReal ll), Just (VInt sn sz vr 0)) ->
        Just $ VReal $ ll ** encodeFloat (fitSnSz sn sz vr) 0
      (Just (VInt sn sz 0 0), Just (VInt _ _ 0 0)) -> Just $ VInt sn sz 1 0
      (Just (VInt sn szl 0 0), Just (VInt True szr v 0)) | fitSnSz True szr v < 0 ->
        Just $ let bm = onesbm szl in VInt sn szl bm bm
      (Just (VInt sn sz _ m), Just (VInt _ _ _ _)) | m /= 0 ->
        Just $ let bm = onesbm sz in VInt sn sz bm bm
      (Just (VInt sn sz _ _), Just (VInt _ _ _ m)) | m /= 0 ->
        Just $ let bm = onesbm sz in VInt sn sz bm bm
      (Just (VInt snl szl vl 0), Just (VInt snr szr vr 0)) ->
        let v = fromRational (toRational $ fitSnSz snl szl vl) ^^ fitSnSz snr szr vr in
        Just $ VInt snl szl (fitSz szl $ truncate v) 0
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      _ -> binerr
    EBLSL -> binIntLoose l r $ \sn sz vl ml vr mr -> uncurry (VInt sn sz) $ case () of
      () | mr /= 0 -> let bm = onesbm sz in (bm, bm)
      () -> let rr = fromEnum vr in (fitSz sz $ vl .<<. rr, fitSz sz $ ml .<<. rr)
    EBLSR -> binIntLoose l r $ \sn sz vl ml vr mr ->
      uncurry (VInt sn sz) $
        if mr /= 0
          then let bm = onesbm sz in (bm, bm)
          else let rr = fromEnum vr in (vl .>>. rr, ml .>>. rr)
    EBASL -> binIntLoose l r $ \sn sz vl ml vr mr -> uncurry (VInt sn sz) $ case () of
      () | mr /= 0 -> let bm = onesbm sz in (bm, bm)
      () -> let rr = fromEnum vr in (fitSz sz $ vl .<<. rr, fitSz sz $ ml .<<. rr)
    EBASR -> binIntLoose l r $ \sn sz vl ml vr mr -> uncurry (VInt sn sz) $ case () of
      () | mr /= 0 -> let bm = onesbm sz in (bm, bm)
      () | sz /= 0 ->
        let rr = fromEnum vr
            isz = fromEnum sz
            bm = (bit rr - 1) .<<. isz
            f x = (x .>>. rr) .|. if testBit x isz then bm else 0
         in (f vl, f ml)
      () -> let rr = fromEnum vr in (vl .>>. rr, ml .>>. rr)
  EECond c _ t f -> evalExpr c >>= \v -> case evalTruth v of
    ZOXZ -> evalExpr f
    ZOXO -> evalExpr t
    ZOXX -> binIntReal f t (\_ _ -> VReal 0) $ \sn sz vf mf vt mt ->
      let nm = mf .|. mt .|. (vf .^. vt) in VInt sn sz ((vf .&. vt) .|. complement nm) nm
  where
    boolInt b = if b then 1 else 0
    mk1 t = evalCast t . uncurry (VInt False 1) . bimap boolInt boolInt
    binA f sn sz vl ml vr mr =
      uncurry (VInt sn sz) $
        if ml == 0 && mr == 0 then f sn sz vl vr else let bm = onesbm sz in (bm, bm)
    binX f sn sz vl ml vr mr = let b = ml /= 0 || mr /= 0 in (b || f sn sz vl vr, b)
    binIntReal l r f g = case (evalExpr l, evalExpr r) of
      (Just (VReal vl), Just (VReal vr)) -> Just $ f vl vr
      (Nothing, Just (VReal _)) -> Nothing
      (Just (VReal _), Nothing) -> Nothing
      (Just (VInt snl szl vl ml), Just (VInt snr szr vr mr)) | snl == snr && szl == szr ->
        Just $ g snl szl vl ml vr mr
      (Nothing, Just (VInt _ _ _ _)) -> Nothing
      (Just (VInt _ _ _ _), Nothing) -> Nothing
      _ -> error "Invalid pair of operands"
    binerr = error "Invalid argument to binary operation"
    binIntLoose l r f = case (evalExpr l, evalExpr r) of
      (Just (VInt sn sz vl ml), Just (VInt _ _ vr mr)) -> Just $ f sn sz vl ml vr mr
      (Nothing, Just (VInt _ _ _ _)) -> Nothing
      (Just (VInt _ _ _ _), Nothing) -> Nothing
      _ -> binerr
    binIntStrict l r f = case (evalExpr l, evalExpr r) of
      (Just (VInt snl szl vl ml), Just (VInt snr szr vr mr)) | snl == snr && szl == szr ->
        Just $ f snl szl vl ml vr mr
      (Nothing, Just (VInt _ _ _ _)) -> Nothing
      (Just (VInt _ _ _ _), Nothing) -> Nothing
      _ -> binerr

evalNExprSelfDet :: NExpr -> Maybe Value
evalNExprSelfDet e = let ElabNExpr ee = elabNExprSelfDet e in evalExpr ee

evalCExprSelfDet :: CExpr -> Maybe Value
evalCExprSelfDet e = let ElabCExpr ee = elabCExprSelfDet e in evalExpr ee

evalNExprCtxDet :: Type -> NExpr -> Maybe Value
evalNExprCtxDet t e = let ElabNExpr ee = elabNExprCtxDet t e in evalExpr ee

evalCExprCtxDet :: Type -> CExpr -> Maybe Value
evalCExprCtxDet t e = let ElabCExpr ee = elabCExprCtxDet t e in evalExpr ee

{-
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
  | Task [(Bool, [AttrIded (TFBlockDecl Dir ElabCExpr)], Attributed (Maybe Statement))]
  | TaskPort [(Dir, ComType Bool ElabCExpr)]
  | FunPort [ComType Bool ElabCExpr]
  | StatementBlock [([AttrIded StdBlockDecl], Bool, [Attributed Statement])]
  | FStatementBlock [([AttrIded StdBlockDecl], Bool, [Attributed FunctionStatement])]
  | Parameter [Parameter ElabCExpr]
  | Specparam [MinTypMax ElabCExpr]
  | GenVar
  | Net [(NetType, NetProp)]
  | TriReg [NetProp]
  | Variable [StdBlockDecl]
-}
