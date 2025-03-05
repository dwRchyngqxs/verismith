-- Module      : Verismith.Verilog2005.AST
-- Description : Verilog 2005 AST.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Verismith.Verilog2005.AST
  ( MinTypMax (..),
    Identifier (..),
    Identified (..),
    UnaryOperator (..),
    BinaryOperator (..),
    Number (..),
    Prim (..),
    HierIdent (..),
    DimRange (..),
    Expr (..),
    CExpr (..),
    NExpr (..),
    MPExpr,
    Attribute (..),
    Attributes,
    Attributed (..),
    AttrIded (..),
    Range2 (..),
    RangeExpr (..),
    NumIdent (..),
    Delay3 (..),
    Delay2 (..),
    Delay1 (..),
    SignRange (..),
    SpecTerm (..),
    EventPrefix (..),
    Dir (..),
    AbsType (..),
    ComType (..),
    NetType (..),
    Strength (..),
    DriveStrength (..),
    dsDefault,
    ChargeStrength (..),
    LValue (..),
    Assign (..),
    Parameter (..),
    ParamOver (..),
    ParamAssign (..),
    PortAssign (..),
    EventPrim (..),
    EventControl (..),
    DelayEventControl (..),
    ProcContAssign (..),
    LoopStatement (..),
    CaseItem (..),
    FunctionStatement (..),
    Statement (..),
    NInputType (..),
    EdgeDesc (..),
    InstanceName (..),
    GICMos (..),
    GIEnable (..),
    GIMos (..),
    GINIn (..),
    GINOut (..),
    GIPassEn (..),
    GIPass (..),
    GIPull (..),
    TimingCheckEvent (..),
    ControlledTimingCheckEvent (..),
    STCArgs (..),
    STCAddArgs (..),
    ModulePathCondition (..),
    SpecPath (..),
    PathDelayValue (..),
    SpecifyItem (..),
    SpecifySingleItem,
    SpecifyBlockedItem,
    SpecParamDecl (..),
    NetProp (..),
    NetDecl (..),
    NetInit (..),
    BlockDecl (..),
    StdBlockDecl (..),
    TFBlockDecl (..),
    GenCaseItem (..),
    UDPInst (..),
    ModInst (..),
    UknInst (..),
    ModGenCondItem (..),
    GenerateCondBlock (..),
    Gate (..),
    ModGenItem (..),
    ModGenBlockedItem,
    ModGenSingleItem,
    ModuleItem (..),
    GenerateBlock (..),
    ModuleBlock (..),
    SigLevel (..),
    ZOX (..),
    CombRow (..),
    Edge (..),
    SeqIn (..),
    SeqRow (..),
    PrimTable (..),
    PrimPort (..),
    PrimitiveBlock (..),
    Dot1Ident (..),
    Cell_inst (..),
    LLU (..),
    ConfigItem (..),
    ConfigBlock (..),
    PVerilog2005 (..),
    Verilog2005,
    SystemFunction (..),
    Logic (..),
    sfMap,
    BXZ (..),
    OXZ (..),
    HXZ (..),
    hiPath,
    mbIdent,
    pbIdent,
    _CIInst,
    ciCell_inst,
    cbIdent,
    cbBody,
  )
where

import Control.Lens
import Data.Functor.Compose
import Data.Functor.Classes
import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w, packChars)
import Data.Data
import Data.Bifunctor
import Data.Data.Lens
import Data.String (IsString (..))
import Text.Show (showListWith)
import Text.Printf (printf)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Vector.Unboxed as V
import GHC.Generics (Generic)
import Numeric.Natural
import Verismith.Utils (Data1)
import Verismith.Verilog2005.Token (BXZ (..), HXZ (..), OXZ (..), ZOX (..))

-- | Minimum, Typical, Maximum
data MinTypMax e
  = MTMSingle !e
  | MTMFull
      { _mtmMin :: !e,
        _mtmTyp :: !e,
        _mtmMax :: !e
      }
  deriving (Show, Eq, Data, Generic, Functor, Foldable, Traversable)

-- | Identifier, do not use for other things (like a string literal), used for biplate
newtype Identifier = Identifier ByteString
  deriving (Show, Eq, Data, Generic)

instance IsString Identifier where
  fromString =
    Identifier . packChars . concatMap
      (\c -> if ' ' < c && c <= '~' then [c] else printf "\\%02x" c)

-- | Quickly add an identifier to all members of a sum type, other uses are discouraged
data Identified t = Identified {_identIdent :: !Identifier, _identData :: !t}
  deriving (Show, Eq, Data, Generic, Functor, Foldable, Traversable)

instance Data1 Identified

showHelper :: (Int -> a -> ShowS) -> Identified a -> ShowS
showHelper fp (Identified i x) = showString "Identified " . shows i . showChar ' ' . fp 0 x

instance Show1 Identified where
  liftShowsPrec fp _ p = showHelper fp
  liftShowList fp _ = showListWith $ showHelper fp
instance Eq1 Identified where
  liftEq f (Identified ia a) (Identified ib b) = ia == ib && f a b

-- | Unary operators
data UnaryOperator
  = UnPlus
  | UnMinus
  | UnLNot
  | UnNot
  | UnAnd
  | UnNand
  | UnOr
  | UnNor
  | UnXor
  | UnXNor
  deriving (Eq, Data, Generic, Enum, Bounded)

instance Show UnaryOperator where
  show x = case x of
    UnPlus -> "+"
    UnMinus -> "-"
    UnLNot -> "!"
    UnNot -> "~"
    UnAnd -> "&"
    UnNand -> "~&"
    UnOr -> "|"
    UnNor -> "~|"
    UnXor -> "^"
    UnXNor -> "~^"

-- | Binary operators
data BinaryOperator
  = BinPlus
  | BinMinus
  | BinTimes
  | BinDiv
  | BinMod
  | BinEq
  | BinNEq
  | BinCEq
  | BinCNEq
  | BinLAnd
  | BinLOr
  | BinLT
  | BinLEq
  | BinGT
  | BinGEq
  | BinAnd
  | BinOr
  | BinXor
  | BinXNor
  | BinPower
  | BinLSL
  | BinLSR
  | BinASL
  | BinASR
  deriving (Eq, Data, Generic, Enum, Bounded)

instance Show BinaryOperator where
  show x = case x of
    BinPlus -> "+"
    BinMinus -> "-"
    BinTimes -> "*"
    BinDiv -> "/"
    BinMod -> "%"
    BinEq -> "=="
    BinNEq -> "!="
    BinCEq -> "==="
    BinCNEq -> "!=="
    BinLAnd -> "&&"
    BinLOr -> "||"
    BinLT -> "<"
    BinLEq -> "<="
    BinGT -> ">"
    BinGEq -> ">="
    BinAnd -> "&"
    BinOr -> "|"
    BinXor -> "^"
    BinXNor -> "~^"
    BinPower -> "**"
    BinLSL -> "<<"
    BinLSR -> ">>"
    BinASL -> "<<<"
    BinASR -> ">>>"

data Number
  = NBinary !(NonEmpty BXZ)
  | NOctal !(NonEmpty OXZ)
  | NDecimal !Natural
  | NHex !(NonEmpty HXZ)
  | NXZ !Bool
  deriving (Show, Eq, Data, Generic)

-- | Parametric primary expression
data Prim i r a
  = PrimNumber
      { _pnSize :: !Natural, -- 0 means unspecified
        _pnSigned :: !Bool,
        _pnValue :: !Number
      }
  | PrimReal !ByteString
  | PrimIdent
      { _piIdent :: !i,
        _piSub :: !r
      }
  | PrimConcat !(NonEmpty (Expr i r a))
  | PrimMultConcat
      { _pmcMul :: !(Expr Identifier (Maybe (RangeExpr CExpr CExpr)) a),
        _pmcExpr :: !(NonEmpty (Expr i r a))
      }
  | PrimFun
      { _pfIdent :: !i,
        _pfAttr :: !a,
        _pfArg :: ![Expr i r a]
      }
  | PrimSysFun
      { _psfIdent :: !ByteString,
        _psfArg :: ![Expr i r a]
      }
  | PrimMinTypMax !(MinTypMax (Expr i r a))
  | PrimString !ByteString
  deriving (Show, Eq, Data, Generic)

-- | Hierarchical identifier
data HierIdent ce = HierIdent
  { _hiPath :: ![(Identifier, Maybe ce)],
    _hiIdent :: !Identifier
  }
  deriving (Show, Eq, Data, Generic, Functor, Foldable, Traversable)

-- | Indexing for dimension and range
data DimRange et ce = DimRange {_drDim :: ![et], _drRange :: !(RangeExpr et ce)}
  deriving (Show, Eq, Data, Generic, Functor)

instance Bifunctor DimRange where
  bimap fe fce (DimRange d r) = DimRange (map fe d) (bimap fe fce r)

-- | Parametric expression
data Expr i r a
  = ExprPrim !(Prim i r a)
  | ExprUnOp
      { _euOp :: !UnaryOperator,
        _euAttr :: !a,
        _euPrim :: !(Prim i r a)
      }
  | ExprBinOp
      { _ebLhs :: !(Expr i r a),
        _ebOp :: !BinaryOperator,
        _ebAttr :: !a,
        _ebRhs :: !(Expr i r a)
      }
  | ExprCond
      { _ecCond :: !(Expr i r a),
        _ecAttr :: !a,
        _ecTrue :: !(Expr i r a),
        _ecFalse :: !(Expr i r a)
      }
  deriving (Show, Eq, Data, Generic)

instance (Data i, Data r, Data a) => Plated (Expr i r a) where
  plate = uniplate

newtype CExpr = CExpr (Expr Identifier (Maybe (RangeExpr CExpr CExpr)) Attributes)
  deriving (Show, Eq, Data, Generic)

newtype NExpr = NExpr (Expr (HierIdent CExpr) (Maybe (DimRange NExpr CExpr)) Attributes)
  deriving (Show, Eq, Data, Generic)

type MPExpr = Expr Identifier () Attributes

-- | Attributes which can be set to various nodes in the AST.
data Attribute = Attribute
  { _attrIdent :: !ByteString,
    _attrValue :: !(Maybe (Expr Identifier (Maybe (RangeExpr CExpr CExpr)) ()))
  }
  deriving (Show, Eq, Data, Generic)

type Attributes = [[Attribute]]

data Attributed t = Attributed {_attrAttr :: !Attributes, _attrData :: !t}
  deriving (Show, Eq, Data, Generic, Functor, Foldable, Traversable)

instance Applicative Attributed where
  pure = Attributed []
  (<*>) (Attributed a1 f) (Attributed a2 x) = Attributed (a1 <> a2) $ f x

data AttrIded t = AttrIded {_aiAttr :: !Attributes, _aiIdent :: !Identifier, _aiData :: !t}
  deriving (Show, Eq, Data, Generic, Functor)

-- | Range2
data Range2 ce = Range2 {_r2MSB :: !ce, _r2LSB :: !ce}
  deriving (Show, Eq, Data, Generic, Functor)

-- | Range expressions
data RangeExpr et ce
  = RESingle !et
  | REPair !(Range2 ce)
  | REBaseOff
      { _reBase :: !et,
        _reMin_plus :: !Bool,
        _reOffset :: !ce
      }
  deriving (Show, Eq, Data, Generic, Functor)

instance Bifunctor RangeExpr where
  bimap fe fce x = case x of
    RESingle e -> RESingle $ fe e
    REPair r -> REPair $ fmap fce r
    REBaseOff be b oe -> REBaseOff (fe be) b (fce oe)

-- | Number or Identifier
data NumIdent
  = NIIdent !Identifier
  | NIReal !ByteString
  | NINumber !Natural
  deriving (Show, Eq, Data, Generic)

-- | Delay3
data Delay3 e
  = D3Base !NumIdent
  | D31 !(MinTypMax e)
  | D32 { _d32Rise :: !(MinTypMax e), _d32Fall :: !(MinTypMax e) }
  | D33
      { _d33Rise :: !(MinTypMax e),
        _d33Fall :: !(MinTypMax e),
        _d33HighZ :: !(MinTypMax e)
      }
  deriving (Show, Eq, Data, Generic)

-- | Delay2
data Delay2 e
  = D2Base !NumIdent
  | D21 !(MinTypMax e)
  | D22 { _d22Rise :: !(MinTypMax e), _d22Fall :: !(MinTypMax e) }
  deriving (Show, Eq, Data, Generic)

-- | Delay1
data Delay1 e
  = D1Base !NumIdent
  | D11 !(MinTypMax e)
  deriving (Show, Eq, Data, Generic)

-- | Signedness and range are often together
data SignRange ce = SignRange {_srSign :: !Bool, _srRange :: !(Maybe (Range2 ce))}
  deriving (Show, Eq, Data, Generic)

-- | Specify terminal
data SpecTerm ce = SpecTerm {_stIdent :: !Identifier, _stRange :: !(Maybe (RangeExpr ce ce))}
  deriving (Show, Eq, Data, Generic)

-- | Event expression prefix
data EventPrefix = EPAny | EPPos | EPNeg
  deriving (Show, Eq, Bounded, Enum, Data, Generic)

-- | Port datatransfer directions
data Dir = DirIn | DirOut | DirInOut
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show Dir where
  show x = case x of DirIn -> "input"; DirOut -> "output"; DirInOut -> "inout"

-- | Abstract types for variables, parameters, functions and tasks
data AbsType = ATInteger | ATReal | ATRealtime | ATTime
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show AbsType where
  show x = case x of
    ATInteger -> "integer"
    ATReal -> "real"
    ATRealtime -> "realtime"
    ATTime -> "time"

-- | Function, parameter and task type
data ComType t ce
  = CTAbstract !AbsType
  | CTConcrete
    { _ctcExtra :: !t,
      _ctcSignRange :: !(SignRange ce)
    }
  deriving (Show, Eq, Data, Generic)

-- | Net type
data NetType
  = NTSupply1
  | NTSupply0
  | NTTri
  | NTTriAnd
  | NTTriOr
  | NTTri1
  | NTTri0
  | NTUwire
  | NTWire
  | NTWAnd
  | NTWOr
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show NetType where
  show x = case x of
    NTSupply1 -> "supply1"
    NTSupply0 -> "supply0"
    NTTri -> "tri"
    NTTriAnd -> "triand"
    NTTriOr -> "trior"
    NTTri1 -> "tri1"
    NTTri0 -> "tri0"
    NTUwire -> "uwire"
    NTWire -> "wire"
    NTWAnd -> "wand"
    NTWOr -> "wor"

-- | Net drive strengths
data Strength = StrSupply | StrStrong {-default-} | StrPull | StrWeak
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show Strength where
  show x = case x of
    StrSupply -> "supply"
    StrStrong -> "strong"
    StrPull -> "pull"
    StrWeak -> "weak"

data DriveStrength
  = DSNormal
      { _ds0 :: !Strength,
        _ds1 :: !Strength
      }
  | DSHighZ
      { _dsHZ :: !Bool,
        _dsStr :: !Strength
      }
  deriving (Show, Eq, Data, Generic)

dsDefault = DSNormal {_ds0 = StrStrong, _ds1 = StrStrong}

-- | Capacitor charge
data ChargeStrength = CSSmall | CSMedium {-default-} | CSLarge
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show ChargeStrength where
  show x = case x of CSSmall -> "(small)"; CSMedium -> "(medium)"; CSLarge -> "(large)"

-- | Left side of assignments
data LValue et ce
  = LVSingle
      { _lvIdent :: !(HierIdent ce),
        _lvDimRange :: !(Maybe (DimRange et ce))
      }
  | LVConcat !(NonEmpty (LValue et ce))
  deriving (Show, Eq, Data, Generic)

-- | Assignment
data Assign et ce e = Assign {_aLValue :: !(LValue et ce), _aValue :: !e}
  deriving (Show, Eq, Data, Generic)

-- | Parameter
data Parameter ce = Parameter {_paramType :: !(ComType () ce), _paramValue :: !(MinTypMax ce)}
  deriving (Show, Eq, Data, Generic)

-- | DefParam assignment
data ParamOver ce = ParamOver {_poIdent :: !(HierIdent ce), _poValue :: !(MinTypMax ce)}
  deriving (Show, Eq, Data, Generic)

-- | Parameter assignment list
data ParamAssign e
  = ParamPositional ![e]
  | ParamNamed ![Identified (Maybe (MinTypMax e))]
  deriving (Show, Eq, Data, Generic)

-- | Port assignment list
data PortAssign e
  = PortNamed ![AttrIded (Maybe e)]
  | PortPositional ![Attributed (Maybe e)]
  deriving (Show, Eq, Data, Generic)

-- | Event primitive
data EventPrim e = EventPrim {_epOp :: !EventPrefix, _epExpr :: !e}
  deriving (Show, Eq, Data, Generic)

-- | Event control
data EventControl e ce
  = ECIdent !(HierIdent ce)
  | ECExpr !(NonEmpty (EventPrim e))
  | ECDeps
  deriving (Show, Eq, Data, Generic)

-- | Delay or Event control
data DelayEventControl e ce
  = DECDelay !(Delay1 e)
  | DECEvent !(EventControl e ce)
  | DECRepeat
      { _decrExpr :: !e,
        _decrEvent :: !(EventControl e ce)
      }
  deriving (Show, Eq, Data, Generic)

-- | Procedural continuous assignment
data ProcContAssign e ce
  = PCAAssign !(Assign e ce e)
  | PCADeassign !(LValue e ce)
  | PCAForce !(Either (Assign e ce e) (Assign ce ce e))
  | PCARelease !(Either (LValue e ce) (LValue ce ce))
  deriving (Show, Eq, Data, Generic)

-- | Loop statement
data LoopStatement e ce
  = LSForever
  | LSRepeat !e
  | LSWhile !e
  | LSFor
      { _lsfInit :: !(Assign e ce e),
        _lsfCond :: !e,
        _lsfUpd :: !(Assign e ce e)
      }
  deriving (Show, Eq, Data, Generic)

-- | Case item
data CaseItem s e = CaseItem {_ciPat :: !(NonEmpty e), _ciVal :: !(Attributed (Maybe s))}
  deriving (Show, Eq, Data, Generic)

-- | Function statement, more limited than general statement because they are purely combinational
data FunctionStatement e ce
  = FSBlockAssign !(Assign e ce e)
  | FSCase
      { _fscType :: !ZOX,
        _fscExpr :: !e,
        _fscBody :: ![CaseItem (FunctionStatement e ce) e],
        _fscDef :: !(Attributed (Maybe (FunctionStatement e ce)))
      }
  | FSIf
      { _fsiExpr :: !e,
        _fsiTrue :: !(Attributed (Maybe (FunctionStatement e ce))),
        _fsiFalse :: !(Attributed (Maybe (FunctionStatement e ce)))
      }
  | FSDisable !(HierIdent ce)
  | FSLoop
      { _fslHead :: !(LoopStatement e ce),
        _fslBody :: !(Attributed (FunctionStatement e ce))
      }
  | FSBlock
      { _fsbHeader :: !(Maybe (Identifier, [AttrIded (StdBlockDecl ce)])),
        _fsbPar_seq :: !Bool,
        _fsbBody :: ![Attributed (FunctionStatement e ce)]
      }
  deriving (Show, Eq, Data, Generic)

instance (Data e, Data ce) => Plated (FunctionStatement e ce) where
  plate = uniplate

-- | Statement
data Statement e ce
  = SBlockAssign
      { _sbaBlock :: !Bool,
        _sbaAssign :: !(Assign e ce e),
        _sbaDelev :: !(Maybe (DelayEventControl e ce))
      }
  | SCase
      { _scType :: !ZOX,
        _scExpr :: !e,
        _scBody :: ![CaseItem (Statement e ce) e],
        _scDef :: !(Attributed (Maybe (Statement e ce)))
      }
  | SIf
      { _siExpr :: !e,
        _siTrue :: !(Attributed (Maybe (Statement e ce))),
        _siFalse :: !(Attributed (Maybe (Statement e ce)))
      }
  | SDisable !(HierIdent ce)
  | SEventTrigger
      { _setIdent :: !(HierIdent ce),
        _setIndex :: ![e]
      }
  | SLoop
      { _slHead :: !(LoopStatement e ce),
        _slBody :: !(Attributed (Statement e ce))
      }
  | SProcContAssign !(ProcContAssign e ce)
  | SProcTimingControl
      { _sptcControl :: !(Either (Delay1 e) (EventControl e ce)),
        _sptcStmt :: !(Attributed (Maybe (Statement e ce)))
      }
  | SBlock
      { _sbHeader :: !(Maybe (Identifier, [AttrIded (StdBlockDecl ce)])),
        _sbPar_seq :: !Bool,
        _sbBody :: ![Attributed (Statement e ce)]
      }
  | SSysTaskEnable
      { _ssteIdent :: !ByteString,
        _ssteArgs :: ![Maybe e]
      }
  | STaskEnable
      { _steIdent :: !(HierIdent ce),
        _steArgs :: ![e]
      }
  | SWait
      { _swExpr :: !e,
        _swStmt :: !(Attributed (Maybe (Statement e ce)))
      }
  deriving (Show, Eq, Data, Generic)

instance (Data e, Data ce) => Plated (Statement e ce) where
  plate = uniplate

-- | N-input logic gate types
data NInputType = NITAnd | NITOr | NITXor
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show NInputType where
  show x = case x of NITAnd -> "and"; NITOr -> "or"; NITXor -> "xor"

-- | Instance name
data InstanceName ce = InstanceName { _inIdent :: !Identifier, _inRange :: !(Maybe (Range2 ce)) }
  deriving (Show, Eq, Data, Generic)

-- | Gate instances
data GICMos e ce = GICMos
  { _gicmName :: !(Maybe (InstanceName ce)),
    _gicmOutput :: !(LValue ce ce),
    _gicmInput :: !e,
    _gicmNControl :: !e,
    _gicmPControl :: !e
  }
  deriving (Show, Eq, Data, Generic)

data GIEnable e ce = GIEnable
  { _gieName :: !(Maybe (InstanceName ce)),
    _gieOutput :: !(LValue ce ce),
    _gieInput :: !e,
    _gieEnable :: !e
  }
  deriving (Show, Eq, Data, Generic)

data GIMos e ce = GIMos
  { _gimName :: !(Maybe (InstanceName ce)),
    _gimOutput :: !(LValue ce ce),
    _gimInput :: !e,
    _gimEnable :: !e
  }
  deriving (Show, Eq, Data, Generic)

data GINIn e ce = GINIn
  { _giniName :: !(Maybe (InstanceName ce)),
    _giniOutput :: !(LValue ce ce),
    _giniInput :: !(NonEmpty e)
  }
  deriving (Show, Eq, Data, Generic)

data GINOut e ce = GINOut
  { _ginoName :: !(Maybe (InstanceName ce)),
    _ginoOutput :: !(NonEmpty (LValue ce ce)),
    _ginoInput :: !e
  }
  deriving (Show, Eq, Data, Generic)

data GIPassEn e ce = GIPassEn
  { _gipeName :: !(Maybe (InstanceName ce)),
    _gipeLhs :: !(LValue ce ce),
    _gipeRhs :: !(LValue ce ce),
    _gipeEnable :: !e
  }
  deriving (Show, Eq, Data, Generic)

data GIPass e ce = GIPass
  { _gipsName :: !(Maybe (InstanceName ce)),
    _gipsLhs :: !(LValue ce ce),
    _gipsRhs :: !(LValue ce ce)
  }
  deriving (Show, Eq, Data, Generic)

data GIPull e ce = GIPull
  { _giplName :: !(Maybe (InstanceName ce)),
    _giplOutput :: !(LValue ce ce)
  }
  deriving (Show, Eq, Data, Generic)

-- | Edge descriptor, a 6 Bool array (01, 0x, 10, 1x, x0, x1)
type EdgeDesc = V.Vector Bool

-- | Timing check (controlled) event
data TimingCheckEvent e ce = TimingCheckEvent
  { _tceEvCtl :: !(Maybe EdgeDesc),
    _tceSpecTerm :: !(SpecTerm ce),
    _tceTimChkCond :: !(Maybe (Bool, e))
  }
  deriving (Show, Eq, Data, Generic)

data ControlledTimingCheckEvent e ce = ControlledTimingCheckEvent
  { _ctceEvCtl :: !EdgeDesc,
    _ctceSpecTerm :: !(SpecTerm ce),
    _ctceTimChkCond :: !(Maybe (Bool, e))
  }
  deriving (Show, Eq, Data, Generic)

-- | System timing check common arguments
data STCArgs e ce = STCArgs
  { _stcaDataEvent :: !(TimingCheckEvent e ce),
    _stcaRefEvent :: !(TimingCheckEvent e ce),
    _stcaTimChkLim :: !e,
    _stcaNotifier :: !(Maybe Identifier)
  }
  deriving (Show, Eq, Data, Generic)

-- | Setuphold and Recrem additionnal arguments
data STCAddArgs e ce = STCAddArgs
  { _stcaaTimChkLim :: !e,
    _stcaaStampCond :: !(Maybe (MinTypMax e)),
    _stcaaChkTimCond :: !(Maybe (MinTypMax e)),
    _stcaaDelayedRef :: !(Maybe (Identified (Maybe (MinTypMax ce)))),
    _stcaaDelayedData :: !(Maybe (Identified (Maybe (MinTypMax ce))))
  }
  deriving (Show, Eq, Data, Generic)

-- | Module path condition
data ModulePathCondition mpe
  = MPCCond !mpe
  | MPCNone
  | MPCAlways
  deriving (Show, Eq, Data, Generic)

-- | Specify path declaration
data SpecPath ce
  = SPParallel
      { _sppInput :: !(SpecTerm ce),
        _sppOutput :: !(SpecTerm ce)
      }
  | SPFull
      { _spfInput :: !(NonEmpty (SpecTerm ce)),
        _spfOutput :: !(NonEmpty (SpecTerm ce))
      }
  deriving (Show, Eq, Data, Generic)

-- | Specify Item path delcaration delay value list
data PathDelayValue ce
  = PDV1 !(MinTypMax ce)
  | PDV2 !(MinTypMax ce) !(MinTypMax ce)
  | PDV3 !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce)
  | PDV6
    !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce)
  | PDV12
    !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce)
    !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce) !(MinTypMax ce)
  deriving (Show, Eq, Data, Generic)

-- | Specify block item
-- | f is either Identity or NonEmpty
-- | it is used to abstract between several specify items in a block and a single comma separated one
data SpecifyItem f e ce mpe
  = SISpecParam
    { _sipcRange :: !(Maybe (Range2 ce)),
      _sipcDecl :: !(f (SpecParamDecl ce))
    }
  | SIPulsestyleOnevent !(f (SpecTerm ce))
  | SIPulsestyleOndetect !(f (SpecTerm ce))
  | SIShowcancelled !(f (SpecTerm ce))
  | SINoshowcancelled !(f (SpecTerm ce))
  | SIPathDeclaration
      { _sipdCond :: !(ModulePathCondition mpe),
        _sipdConn :: !(SpecPath ce),
        _sipdPolarity :: !(Maybe Bool),
        _sipdEDS :: !(Maybe (e, Maybe Bool)),
        _sipdValue :: !(PathDelayValue ce)
      }
  | SISetup !(STCArgs e ce)
  | SIHold !(STCArgs e ce)
  | SISetupHold
      { _sishArgs :: !(STCArgs e ce),
        _sishAddArgs :: !(STCAddArgs e ce)
      }
  | SIRecovery !(STCArgs e ce)
  | SIRemoval !(STCArgs e ce)
  | SIRecrem
      { _sirArgs :: !(STCArgs e ce),
        _sirAddArgs :: !(STCAddArgs e ce)
      }
  | SISkew !(STCArgs e ce)
  | SITimeSkew
      { _sitsArgs :: !(STCArgs e ce),
        _sitsEvBased :: !(Maybe ce),
        _sitsRemActive :: !(Maybe ce)
      }
  | SIFullSkew
      { _sifsArgs :: !(STCArgs e ce),
        _sifsTimChkLim :: !e,
        _sifsEvBased :: !(Maybe ce),
        _sifsRemActive :: !(Maybe ce)
      }
  | SIPeriod
      { _sipCRefEvent :: !(ControlledTimingCheckEvent e ce),
        _sipTimCtlLim :: !e,
        _sipNotif :: !(Maybe Identifier)
      }
  | SIWidth
      { _siwRefEvent :: !(ControlledTimingCheckEvent e ce),
        _siwTimCtlLim :: !e,
        _siwThresh :: !(Maybe ce),
        _siwNotif :: !(Maybe Identifier)
      }
  | SINoChange
      { _sincRefEvent :: !(TimingCheckEvent e ce),
        _sincDataEvent :: !(TimingCheckEvent e ce),
        _sincStartEdgeOff :: !(MinTypMax e),
        _sincEndEdgeOff :: !(MinTypMax e),
        _sincNotif :: !(Maybe Identifier)
      }
  deriving (Generic)

deriving instance (Show1 f, Show e, Show ce, Show mpe) => Show (SpecifyItem f e ce mpe)
deriving instance (Eq1 f, Eq e, Eq ce, Eq mpe) => Eq (SpecifyItem f e ce mpe)
deriving instance (Data e, Data ce, Data mpe, Data1 f) => Data (SpecifyItem f e ce mpe)

type SpecifySingleItem = SpecifyItem NonEmpty
type SpecifyBlockedItem = SpecifyItem Identity

-- | Specparam declaration
data SpecParamDecl ce
  = SPDAssign
    { _spdaIdent :: !Identifier,
      _spdaValue :: !(MinTypMax ce)
    }
  | SPDPathPulse -- Not completely accurate input/output as it is ambiguous
      { _spdpInOut :: !(Maybe (SpecTerm ce, SpecTerm ce)),
        _spdpReject :: !(MinTypMax ce),
        _spdpError :: !(MinTypMax ce)
      }
  deriving (Show, Eq, Data, Generic)

-- | Net common properties
data NetProp e ce = NetProp
  { _npSigned :: !Bool,
    _npVector :: !(Maybe (Maybe Bool, Range2 ce)),
    _npDelay :: !(Maybe (Delay3 e))
  }
  deriving (Show, Eq, Data, Generic)

-- | Net declaration
data NetDecl ce = NetDecl {_ndIdent :: !Identifier, _ndDim :: ![Range2 ce]}
  deriving (Show, Eq, Data, Generic)

-- | Net initialisation
data NetInit e = NetInit {_niIdent :: !Identifier, _niValue :: !e}
  deriving (Show, Eq, Data, Generic)

-- | Block declaration
-- | t is used to abstract between block_decl and modgen_decl
-- | f is used to abstract between the separated and grouped modgen_item
data BlockDecl f t ce
  = BDReg
    { _bdrgSR :: !(SignRange ce),
      _bdrgData :: !(f t)
    }
  | BDInt !(f t)
  | BDReal !(f t)
  | BDTime !(f t)
  | BDRealTime !(f t)
  | BDEvent !(f [Range2 ce])
  | BDLocalParam
    { _bdlpType :: !(ComType () ce),
      _bdlpValue :: !(f (MinTypMax ce))
    }
  deriving (Generic)

deriving instance (Show1 f, Show e, Show ce) => Show (BlockDecl f e ce)
deriving instance (Eq1 f, Eq e, Eq ce) => Eq (BlockDecl f e ce)
deriving instance (Data e, Data ce, Data1 f) => Data (BlockDecl f e ce)

-- | Block item declaration (for statement blocks [begin/fork], tasks, and functions)
data StdBlockDecl ce
  = SBDBlockDecl !(BlockDecl Identity [Range2 ce] ce)
  | SBDParameter !(Parameter ce)
  deriving (Show, Eq, Data, Generic)

-- | Task and Function block declaration
data TFBlockDecl t ce
  = TFBDStd !(StdBlockDecl ce)
  | TFBDPort
    { _tfbdpDir :: !t,
      _tfbdpType :: !(ComType Bool ce)
    }
  deriving (Show, Eq, Data, Generic)

-- maybe merge with CaseItem because then GCB could be replaced with Statement
-- | Case generate branch
data GenCaseItem e ce = GenCaseItem
  { _gciPat :: !(NonEmpty ce),
    _gciVal :: !(GenerateCondBlock e ce)
  }
  deriving (Show, Eq, Data, Generic)

-- | UDP named instantiation
data UDPInst e ce = UDPInst
  { _udpiName :: !(Maybe (InstanceName ce)),
    _udpiLValue :: !(LValue ce ce),
    _udpiArgs :: !(NonEmpty e)
  }
  deriving (Show, Eq, Data, Generic)

-- | Module named instantiation
data ModInst e ce = ModInst {_miName :: !(InstanceName ce), _miPort :: !(PortAssign e)}
  deriving (Show, Eq, Data, Generic)

-- | Unknown named instantiation
data UknInst e ce = UknInst
  { _uiName :: !(InstanceName ce),
    _uiArg0 :: !(LValue ce ce),
    _uiArgs :: !(NonEmpty e)
  }
  deriving (Show, Eq, Data, Generic)

-- | Module or Generate conditional item because scoping rules are special
data ModGenCondItem e ce
  = MGCIIf
      { _mgiiExpr :: !ce,
        _mgiiTrue :: !(GenerateCondBlock e ce),
        _mgiiFalse :: !(GenerateCondBlock e ce)
      }
  | MGCICase
      { _mgicExpr :: !ce,
        _mgicBranch :: ![GenCaseItem e ce],
        _mgicDefault :: !(GenerateCondBlock e ce)
      }
  deriving (Show, Eq, Data, Generic)

-- | Generate Block or Conditional Item or nothing because scoping rules are special
data GenerateCondBlock e ce
  = GCBEmpty
  | GCBBlock !(GenerateBlock e ce)
  | GCBConditional !(Attributed (ModGenCondItem e ce))
  deriving (Show, Eq, Data, Generic)

-- | Gate instantiation module item
data Gate f e ce
  = GCMos
      { _gcmR :: !Bool,
        _gcmDelay :: !(Maybe (Delay3 e)),
        _gcmInst :: !(f (GICMos e ce))
      }
  | GEnable
      { _geR :: !Bool,
        _ge1_0 :: !Bool,
        _geStrength :: !DriveStrength,
        _geDelay :: !(Maybe (Delay3 e)),
        _geInst :: !(f (GIEnable e ce))
      }
  | GMos
      { _gmR :: !Bool,
        _gmN_P :: !Bool,
        _gmDelay :: !(Maybe (Delay3 e)),
        _gmInst :: !(f (GIMos e ce))
      }
  | GNIn
      { _gninType :: !NInputType,
        _gninN :: !Bool,
        _gninStrength :: !DriveStrength,
        _gninDelay :: !(Maybe (Delay2 e)),
        _gninInst :: !(f (GINIn e ce))
      }
  | GNOut
      { _gnoR :: !Bool,
        _gnoStrength :: !DriveStrength,
        _gnoDelay :: !(Maybe (Delay2 e)),
        _gnoInst :: !(f (GINOut e ce))
      }
  | GPassEn
      { _gpeR :: !Bool,
        _gpe1_0 :: !Bool,
        _gpeDelay :: !(Maybe (Delay2 e)),
        _gpeInst :: !(f (GIPassEn e ce))
      }
  | GPass
      { _gpsR :: !Bool,
        _gpsInst :: !(f (GIPass e ce))
      }
  | GPull
      { _gplUp_down :: !Bool,
        _gplStrength :: !DriveStrength,
        _gplInst :: !(f (GIPull e ce))
      }
  deriving (Generic)

deriving instance (Show1 f, Show e, Show ce) => Show (Gate f e ce)
deriving instance (Eq1 f, Eq e, Eq ce) => Eq (Gate f e ce)
deriving instance (Data e, Data ce, Data1 f) => Data (Gate f e ce)

-- | Module or Generate item
-- | f is either Identity or NonEmpty
-- | it is used to abstract between several modgen items in a block and a single comma separated one
data ModGenItem f e ce
  = MGINetInit
      { _mginiType :: !NetType,
        _mginiDrive :: !DriveStrength,
        _mginiProp :: !(NetProp e ce),
        _mginiInit :: !(f (NetInit e))
      }
  | MGINetDecl
      { _mgindType :: !NetType,
        _mgindProp :: !(NetProp e ce),
        _mgindDecl :: !(f (NetDecl ce))
      }
  | MGITriD
      { _mgitdDrive :: !DriveStrength,
        _mgitdProp :: !(NetProp e ce),
        _mgitdInit :: !(f (NetInit e))
      }
  | MGITriC
      { _mgitcCharge :: !ChargeStrength,
        _mgitcProp :: !(NetProp e ce),
        _mgitcDecl :: !(f (NetDecl ce))
      }
  | MGIBlockDecl !(BlockDecl (Compose f Identified) (Either [Range2 ce] ce) ce)
  | MGIGenVar !(f Identifier)
  | MGITask
      { _mgitAuto :: !Bool,
        _mgitIdent :: !Identifier,
        _mgitDecl :: ![AttrIded (TFBlockDecl Dir ce)],
        _mgitBody :: !(Attributed (Maybe (Statement e ce)))
      }
  | MGIFunc
      { _mgifAuto :: !Bool,
        _mgifType :: !(Maybe (ComType () ce)),
        _mgifIdent :: !Identifier,
        _mgifDecl :: ![AttrIded (TFBlockDecl () ce)],
        _mgifBody :: !(FunctionStatement e ce)
      }
  | MGIDefParam !(f (ParamOver ce))
  | MGIContAss
      { _mgicaStrength :: !DriveStrength,
        _mgicaDelay :: !(Maybe (Delay3 e)),
        _mgicaAssign :: !(f (Assign ce ce e))
      }
  | MGIGate !(Gate f e ce)
  | MGIUDPInst
      { _mgiudpiUDP :: !Identifier,
        _mgiudpiStrength :: !DriveStrength,
        _mgiudpiDelay :: !(Maybe (Delay2 e)),
        _mgiudpiInst :: !(f (UDPInst e ce))
      }
  | MGIModInst
      { _mgimiMod :: !Identifier,
        _mgimiParams :: !(ParamAssign e),
        _mgimiInst :: !(f (ModInst e ce))
      }
  | MGIUnknownInst -- Sometimes identifying what is instantiated is impossible
      { _mgiuiType :: !Identifier,
        _mgiuiParam :: !(Maybe (Either e (e, e))),
        _mgiuiInst :: !(f (UknInst e ce))
      }
  | MGIInitial !(Attributed (Statement e ce))
  | MGIAlways !(Attributed (Statement e ce))
  | MGILoopGen
      { _mgilgInitIdent :: !Identifier,
        _mgilgInitValue :: !ce,
        _mgilgCond :: !ce,
        _mgilgUpdIdent :: !Identifier,
        _mgilgUpdValue :: !ce,
        _mgilgBody :: !(GenerateBlock e ce)
      }
  | MGICondItem !(ModGenCondItem e ce)
  deriving (Generic)

deriving instance (Show1 f, Show e, Show ce) => Show (ModGenItem f e ce)
deriving instance (Eq1 f, Eq e, Eq ce) => Eq (ModGenItem f e ce)
deriving instance (Data e, Data ce, Data1 f) => Data (ModGenItem f e ce)

type ModGenBlockedItem = ModGenItem Identity
type ModGenSingleItem = ModGenItem NonEmpty

instance (Data e, Data ce, Data1 f) => Plated (ModGenItem f e ce) where
  plate = uniplate

-- | Module item: body of module
-- | Caution: if MIPort sign is False then it can be overriden by a MGINetDecl/Init
data ModuleItem e ce mpe
  = MIMGI !(Attributed (ModGenBlockedItem e ce))
  | MIPort !(AttrIded (Dir, SignRange ce))
  | MIParameter !(AttrIded (Parameter ce))
  | MIGenReg ![Attributed (ModGenBlockedItem e ce)]
  | MISpecParam
    { _mispAttribute :: !Attributes,
      _mispRange :: !(Maybe (Range2 ce)),
      _mispDecl :: !(SpecParamDecl ce)
    }
  | MISpecBlock ![SpecifyBlockedItem e ce mpe]
  deriving (Show, Eq, Data, Generic)

data GenerateBlock e ce = GenerateBlock
  { _gbIdent :: !(Maybe Identifier),
    _gbBody :: ![Attributed (ModGenBlockedItem e ce)]
  }
  deriving (Show, Eq, Data, Generic)

-- | Module block
data ModuleBlock e ce mpe = ModuleBlock
  { _mbAttr :: !Attributes,
    _mbMacro :: !Bool,
    _mbIdent :: !Identifier,
    _mbPortInter :: ![Identified [Identified (Maybe (RangeExpr ce ce))]],
    _mbBody :: ![ModuleItem e ce mpe],
    _mbTimescale :: !(Maybe (Int, Int)),
    _mbCell :: !Bool,
    _mbPull :: !(Maybe Bool),
    _mbDefNetType :: !(Maybe NetType)
  }
  deriving (Show, Eq, Data, Generic)

-- | Signal level
data SigLevel = L0 | L1 | LX | LQ | LB
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show SigLevel where
  show x = case x of L0 -> "0"; L1 -> "1"; LX -> "x"; LQ -> "?"; LB -> "b"

-- | Combinatorial table row
data CombRow = CombRow {_crInput :: !(NonEmpty SigLevel), _crOutput :: !ZOX}
  deriving (Show, Eq, Data, Generic)

-- | Edge specifier
data Edge
  = EdgePos_neg !Bool
  | EdgeDesc
      { _edFrom :: !SigLevel,
        _edTo :: !SigLevel
      }
  deriving (Eq, Data, Generic)

instance Show Edge where
  show x = case x of
    EdgePos_neg b -> if b then "p" else "n"
    EdgeDesc LQ LQ -> "*"
    EdgeDesc f t -> '(' : show f ++ show t ++ ")"

-- | Seqential table inputs: a list of input levels with at most 1 edge specifier
data SeqIn
  = SIComb !(NonEmpty SigLevel)
  | SISeq ![SigLevel] !Edge ![SigLevel]
  deriving (Eq, Data, Generic)

instance Show SeqIn where
  show x = case x of
    SIComb l -> concatMap show l
    SISeq l0 e l1 -> concatMap show l0 ++ show e ++ concatMap show l1

-- | Sequential table row
data SeqRow = SeqRow
  { _srowInput :: !SeqIn,
    _srowState :: !SigLevel,
    _srowNext :: !(Maybe ZOX)
  }
  deriving (Show, Eq, Data, Generic)

-- | Primitive transition table
data PrimTable
  = CombTable !(NonEmpty CombRow)
  | SeqTable
      { _stInit :: !(Maybe ZOX),
        _stRow :: !(NonEmpty SeqRow)
      }
  deriving (Show, Eq, Data, Generic)

-- | Primitive port type
data PrimPort ce
  = PPInput
  | PPOutput
  | PPReg
  | PPOutReg !(Maybe ce) -- no sem
  deriving (Show, Eq, Data, Generic)

-- | Primitive block
data PrimitiveBlock ce = PrimitiveBlock
  { _pbAttr :: !Attributes,
    _pbIdent :: !Identifier,
    _pbOutput :: !Identifier,
    _pbInput :: !(NonEmpty Identifier),
    _pbPortDecl :: !(NonEmpty (AttrIded (PrimPort ce))),
    _pbBody :: !PrimTable
  }
  deriving (Show, Eq, Data, Generic)

-- | Library prefixed cell
data Dot1Ident = Dot1Ident {_d1iLib :: !(Maybe ByteString), _d1iCell :: !Identifier}
  deriving (Show, Eq, Data, Generic)

-- | Cell or instance
data Cell_inst
  = CICell !Dot1Ident
  | CIInst !(NonEmpty Identifier)
  deriving (Show, Eq, Data, Generic)

-- | Liblist or Use
data LLU
  = LLULiblist ![ByteString]
  | LLUUse
      { _lluUIdent :: !Dot1Ident,
        _lluUConfig :: !Bool
      }
  deriving (Show, Eq, Data, Generic)

-- | Items in a config block
data ConfigItem = ConfigItem
  { _ciCell_inst :: !Cell_inst,
    _ciLLU :: !LLU
  }
  deriving (Show, Eq, Data, Generic)

-- | Config Block: Identifier, Design lines, Configuration items
data ConfigBlock = ConfigBlock
  { _cbIdent :: !Identifier,
    _cbDesign :: ![Dot1Ident],
    _cbBody :: ![ConfigItem],
    _cbDef :: ![ByteString]
  }
  deriving (Show, Eq, Data, Generic)

-- | Internal representation of Verilog2005 AST
data PVerilog2005 e ce mpe = Verilog2005
  { _vModule :: ![ModuleBlock e ce mpe],
    _vPrimitive :: ![PrimitiveBlock ce],
    _vConfig :: ![ConfigBlock]
  }
  deriving (Show, Eq, Data, Generic)

type Verilog2005 = PVerilog2005 NExpr CExpr MPExpr

instance Semigroup (PVerilog2005 e ce mpe) where
  (<>) v2a v2b =
    v2a
      { _vModule = _vModule v2a <> _vModule v2b,
        _vPrimitive = _vPrimitive v2a <> _vPrimitive v2b,
        _vConfig = _vConfig v2a <> _vConfig v2b
      }

instance Monoid (PVerilog2005 e ce mpe) where
  mempty = Verilog2005 [] [] []

$(makeLenses ''HierIdent)
$(makeLenses ''ModuleBlock)
$(makeLenses ''PrimitiveBlock)
$(makePrisms ''Cell_inst)
$(makeLenses ''ConfigItem)
$(makeLenses ''ConfigBlock)

data Logic = LAnd | LOr | LNand | LNor
  deriving (Eq, Data)

instance Show Logic where
  show x = case x of LAnd -> "and"; LOr -> "or"; LNand -> "nand"; LNor -> "nor"

data SystemTask
  = STDisplay
  | STDisplayb
  | STDisplayh
  | STDisplayo
  | STStrobe
  | STStrobeb
  | STStrobeh
  | STStrobeo
  | STWrite
  | STWriteb
  | STWriteh
  | STWriteo
  | STMonitor
  | STMonitorb
  | STMonitorh
  | STMonitoro
  | STMonitoroff
  | STMonitoron
  | STFclose
  | STFdisplay
  | STFdisplayb
  | STFdisplayh
  | STFdisplayo
  | STFstrobe
  | STFstrobeb
  | STFstrobeh
  | STFstrobeo
  | STSwrite
  | STSwriteb
  | STSwriteh
  | STSwriteo
  | STFflush
  | STSdfannotate
  | STFwrite
  | STFwriteb
  | STFwriteh
  | STFwriteo
  | STFmonitor
  | STFmonitorb
  | STFmonitorh
  | STFmonitoro
  | STSformat
  | STReadmemb
  | STReadmemh
  | STPrinttimescale
  | STTimeformat
  | STFinish
  | STStop
  | STQinitialize
  | STQremove
  | STQexam
  | STQadd
  | STQfull
  | STPla
      { _stpSync :: !Bool,
        _stpLogic :: !Logic,
        _stpPla_arr :: !Bool
      }
  deriving (Eq, Data)

data SystemFunction
  = SFFscanf
  | SFFread
  | SFFseek
  | SFFeof
  | SFFopen
  | SFFgetc
  | SFUngetc
  | SFFgets
  | SFSscanf
  | SFRewind
  | SFFtell
  | SFFerror
  | SFRealtime
  | SFTime
  | SFStime
  | SFBitstoreal
  | SFItor
  | SFSigned
  | SFRealtobits
  | SFRtoi
  | SFUnsigned
  | SFRandom
  | SFDisterlang
  | SFDistnormal
  | SFDistt
  | SFDistchisquare
  | SFDistexponential
  | SFDistpoisson
  | SFDistuniform
  | SFClog2
  | SFLn
  | SFLog10
  | SFExp
  | SFSqrt
  | SFPow
  | SFFloor
  | SFCeil
  | SFSin
  | SFCos
  | SFTan
  | SFAsin
  | SFAcos
  | SFAtan
  | SFAtan2
  | SFHypot
  | SFSinh
  | SFCosh
  | SFTanh
  | SFAsinh
  | SFAcosh
  | SFAtanh
  | SFTestplusargs
  | SFValueplusargs
  deriving (Eq, Data)

instance Show SystemTask where
  show x = case x of
    STDisplay -> "display"
    STDisplayb -> "displayb"
    STDisplayh -> "displayh"
    STDisplayo -> "displayo"
    STStrobe -> "strobe"
    STStrobeb -> "strobeb"
    STStrobeh -> "strobeh"
    STStrobeo -> "strobeo"
    STWrite -> "write"
    STWriteb -> "writeb"
    STWriteh -> "writeh"
    STWriteo -> "writeo"
    STMonitor -> "monitor"
    STMonitorb -> "monitorb"
    STMonitorh -> "monitorh"
    STMonitoro -> "monitoro"
    STMonitoroff -> "monitoroff"
    STMonitoron -> "monitoron"
    STFclose -> "fclose"
    STFdisplay -> "fdisplay"
    STFdisplayb -> "fdisplayb"
    STFdisplayh -> "fdisplayh"
    STFdisplayo -> "fdisplayo"
    STFstrobe -> "fstrobe"
    STFstrobeb -> "fstrobeb"
    STFstrobeh -> "fstrobeh"
    STFstrobeo -> "fstrobeo"
    STSwrite -> "swrite"
    STSwriteb -> "swriteb"
    STSwriteh -> "swriteh"
    STSwriteo -> "swriteo"
    STFflush -> "fflush"
    STSdfannotate -> "sdf_annotate"
    STFwrite -> "fwrite"
    STFwriteb -> "fwriteb"
    STFwriteh -> "fwriteh"
    STFwriteo -> "fwriteo"
    STFmonitor -> "fmonitor"
    STFmonitorb -> "fmonitorb"
    STFmonitorh -> "fmonitorh"
    STFmonitoro -> "fmonitoro"
    STSformat -> "sformat"
    STReadmemb -> "readmemb"
    STReadmemh -> "readmemh"
    STPrinttimescale -> "printtimescale"
    STTimeformat -> "timeformat"
    STFinish -> "finish"
    STStop -> "stop"
    STQinitialize -> "q_initialize"
    STQremove -> "q_remove"
    STQexam -> "q_exam"
    STQadd -> "q_add"
    STQfull -> "q_full"
    STPla True LAnd False -> "sync$and$array"
    STPla True LAnd True -> "sync$and$plane"
    STPla True LOr False -> "sync$or$array"
    STPla True LOr True -> "sync$or$plane"
    STPla True LNand False -> "sync$nand$array"
    STPla True LNand True -> "sync$nand$plane"
    STPla True LNor False -> "sync$nor$array"
    STPla True LNor True -> "sync$nor$plane"
    STPla False LAnd False -> "async$and$array"
    STPla False LAnd True -> "async$and$plane"
    STPla False LOr False -> "async$or$array"
    STPla False LOr True -> "async$or$plane"
    STPla False LNand False -> "async$nand$array"
    STPla False LNand True -> "async$nand$plane"
    STPla False LNor False -> "async$nor$array"
    STPla False LNor True -> "async$nor$plane"

instance Show SystemFunction where
  show x = case x of
    SFFscanf -> "fscanf"
    SFFread -> "fread"
    SFFseek -> "fseek"
    SFFeof -> "feof"
    SFFopen -> "fopen"
    SFFgetc -> "fgetc"
    SFUngetc -> "ungetc"
    SFFgets -> "gets"
    SFSscanf -> "sscanf"
    SFRewind -> "rewind"
    SFFtell -> "ftell"
    SFFerror -> "ferror"
    SFRealtime -> "realtime"
    SFTime -> "time"
    SFStime -> "stime"
    SFBitstoreal -> "bitstoreal"
    SFItor -> "itor"
    SFSigned -> "signed"
    SFRealtobits -> "realtobits"
    SFRtoi -> "rtoi"
    SFUnsigned -> "unsigned"
    SFRandom -> "random"
    SFDisterlang -> "dist_erlang"
    SFDistnormal -> "dist_normal"
    SFDistt -> "dist_t"
    SFDistchisquare -> "dist_chi_square"
    SFDistexponential -> "dist_exponential"
    SFDistpoisson -> "dist_poisson"
    SFDistuniform -> "dist_uniform"
    SFClog2 -> "clog2"
    SFLn -> "ln"
    SFLog10 -> "log10"
    SFExp -> "exp"
    SFSqrt -> "sqrt"
    SFPow -> "pow"
    SFFloor -> "floor"
    SFCeil -> "ceil"
    SFSin -> "sin"
    SFCos -> "cos"
    SFTan -> "tan"
    SFAsin -> "asin"
    SFAcos -> "acos"
    SFAtan -> "atan"
    SFAtan2 -> "atan2"
    SFHypot -> "hypot"
    SFSinh -> "sinh"
    SFCosh -> "cosh"
    SFTanh -> "tanh"
    SFAsinh -> "asinh"
    SFAcosh -> "acosh"
    SFAtanh -> "atanh"
    SFTestplusargs -> "test$plusargs"
    SFValueplusargs -> "value$plusargs"

stMap :: HashMap.HashMap ByteString SystemTask
stMap =
  HashMap.fromList
    [ ("display", STDisplay),
      ("displayb", STDisplayb),
      ("displayh", STDisplayh),
      ("displayo", STDisplayo),
      ("strobe", STStrobe),
      ("strobeb", STStrobeb),
      ("strobeh", STStrobeh),
      ("strobeo", STStrobeo),
      ("write", STWrite),
      ("writeb", STWriteb),
      ("writeh", STWriteh),
      ("writeo", STWriteo),
      ("monitor", STMonitor),
      ("monitorb", STMonitorb),
      ("monitorh", STMonitorh),
      ("monitoro", STMonitoro),
      ("monitoroff", STMonitoroff),
      ("monitoron", STMonitoron),
      ("fclose", STFclose),
      ("fdisplay", STFdisplay),
      ("fdisplayb", STFdisplayb),
      ("fdisplayh", STFdisplayh),
      ("fdisplayo", STFdisplayo),
      ("fstrobe", STFstrobe),
      ("fstrobeb", STFstrobeb),
      ("fstrobeh", STFstrobeh),
      ("fstrobeo", STFstrobeo),
      ("swrite", STSwrite),
      ("swriteb", STSwriteb),
      ("swriteh", STSwriteh),
      ("swriteo", STSwriteo),
      ("fflush", STFflush),
      ("sdf_annotate", STSdfannotate),
      ("fwrite", STFwrite),
      ("fwriteb", STFwriteb),
      ("fwriteh", STFwriteh),
      ("fwriteo", STFwriteo),
      ("fmonitor", STFmonitor),
      ("fmonitorb", STFmonitorb),
      ("fmonitorh", STFmonitorh),
      ("fmonitoro", STFmonitoro),
      ("sformat", STSformat),
      ("readmemb", STReadmemb),
      ("readmemh", STReadmemh),
      ("printtimescale", STPrinttimescale),
      ("timeformat", STTimeformat),
      ("finish", STFinish),
      ("stop", STStop),
      ("q_initialize", STQinitialize),
      ("q_remove", STQremove),
      ("q_exam", STQexam),
      ("q_add", STQadd),
      ("q_full", STQfull),
      ("sync$and$array", STPla True LAnd False),
      ("sync$and$plane", STPla True LAnd True),
      ("sync$or$array", STPla True LOr False),
      ("sync$or$plane", STPla True LOr True),
      ("sync$nand$array", STPla True LNand False),
      ("sync$nand$plane", STPla True LNand True),
      ("sync$nor$array", STPla True LNor False),
      ("sync$nor$plane", STPla True LNor True),
      ("async$and$array", STPla False LAnd False),
      ("async$and$plane", STPla False LAnd True),
      ("async$or$array", STPla False LOr False),
      ("async$or$plane", STPla False LOr True),
      ("async$nand$array", STPla False LNand False),
      ("async$nand$plane", STPla False LNand True),
      ("async$nor$array", STPla False LNor False),
      ("async$nor$plane", STPla False LNor True)
    ]

sfMap :: HashMap.HashMap ByteString SystemFunction
sfMap =
  HashMap.fromList
    [ ("fscanf", SFFscanf),
      ("fread", SFFread),
      ("fseek", SFFseek),
      ("feof", SFFeof),
      ("fopen", SFFopen),
      ("fgetc", SFFgetc),
      ("ungetc", SFUngetc),
      ("gets", SFFgets),
      ("sscanf", SFSscanf),
      ("rewind", SFRewind),
      ("ftell", SFFtell),
      ("ferror", SFFerror),
      ("realtime", SFRealtime),
      ("time", SFTime),
      ("stime", SFStime),
      ("bitstoreal", SFBitstoreal),
      ("itor", SFItor),
      ("signed", SFSigned),
      ("realtobits", SFRealtobits),
      ("rtoi", SFRtoi),
      ("unsigned", SFUnsigned),
      ("random", SFRandom),
      ("dist_erlang", SFDisterlang),
      ("dist_normal", SFDistnormal),
      ("dist_t", SFDistt),
      ("dist_chi_square", SFDistchisquare),
      ("dist_exponential", SFDistexponential),
      ("dist_poisson", SFDistpoisson),
      ("dist_uniform", SFDistuniform),
      ("clog2", SFClog2),
      ("ln", SFLn),
      ("log10", SFLog10),
      ("exp", SFExp),
      ("sqrt", SFSqrt),
      ("pow", SFPow),
      ("floor", SFFloor),
      ("ceil", SFCeil),
      ("sin", SFSin),
      ("cos", SFCos),
      ("tan", SFTan),
      ("asin", SFAsin),
      ("acos", SFAcos),
      ("atan", SFAtan),
      ("atan2", SFAtan2),
      ("hypot", SFHypot),
      ("sinh", SFSinh),
      ("cosh", SFCosh),
      ("tanh", SFTanh),
      ("asinh", SFAsinh),
      ("acosh", SFAcosh),
      ("atanh", SFAtanh),
      ("test$plusargs", SFTestplusargs),
      ("value$plusargs", SFValueplusargs)
    ]
