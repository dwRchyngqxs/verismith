{-|
Module      : VeriFuzz.Verilog.Gen
Description : Various useful generators.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Various useful generators.
-}

{-# LANGUAGE TemplateHaskell #-}

module VeriFuzz.Verilog.Gen
    ( -- * Generation methods
      procedural
    , proceduralIO
    , randomMod
    )
where

import           Control.Lens                   hiding (Context)
import           Control.Monad                  (replicateM)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader     hiding (local)
import           Control.Monad.Trans.State.Lazy
import           Data.Foldable                  (fold)
import           Data.List.NonEmpty             (toList)
import qualified Data.Text                      as T
import           Hedgehog                       (Gen)
import qualified Hedgehog.Gen                   as Hog
import qualified Hedgehog.Range                 as Hog
import           VeriFuzz.Config
import           VeriFuzz.Internal
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.BitVec
import           VeriFuzz.Verilog.Internal
import           VeriFuzz.Verilog.Mutate

data Context = Context { _variables   :: [Port]
                       , _parameters  :: [Parameter]
                       , _modules     :: [ModDecl]
                       , _nameCounter :: {-# UNPACK #-} !Int
                       , _stmntDepth  :: {-# UNPACK #-} !Int
                       , _modDepth    :: {-# UNPACK #-} !Int
                       }

makeLenses ''Context

type StateGen =  StateT Context (ReaderT Config Gen)

toId :: Int -> Identifier
toId = Identifier . ("w" <>) . T.pack . show

toPort :: Identifier -> Gen Port
toPort ident = do
    i <- range
    return $ wire i ident

sumSize :: [Port] -> Range
sumSize ps = sum $ ps ^.. traverse . portSize

random :: [Identifier] -> (Expr -> ContAssign) -> Gen ModItem
random ctx fun = do
    expr <- Hog.sized (exprWithContext (ProbExpr 1 1 1 1 1 1 0 1 1) ctx)
    return . ModCA $ fun expr

--randomAssigns :: [Identifier] -> [Gen ModItem]
--randomAssigns ids = random ids . ContAssign <$> ids

randomOrdAssigns :: [Identifier] -> [Identifier] -> [Gen ModItem]
randomOrdAssigns inp ids = snd $ foldr generate (inp, []) ids
    where generate cid (i, o) = (cid : i, random i (ContAssign cid) : o)

randomMod :: Int -> Int -> Gen ModDecl
randomMod inps total = do
    x     <- sequence $ randomOrdAssigns start end
    ident <- sequence $ toPort <$> ids
    let inputs_ = take inps ident
    let other   = drop inps ident
    let y = ModCA . ContAssign "y" . fold $ Id <$> drop inps ids
    let yport   = [wire (sumSize other) "y"]
    return . declareMod other $ ModDecl "test_module"
                                        yport
                                        inputs_
                                        (x ++ [y])
                                        []
  where
    ids   = toId <$> [1 .. total]
    end   = drop inps ids
    start = take inps ids

gen :: Gen a -> StateGen a
gen = lift . lift

listOf1 :: Gen a -> Gen [a]
listOf1 a = toList <$> Hog.nonEmpty (Hog.linear 0 100) a

--listOf :: Gen a -> Gen [a]
--listOf = Hog.list (Hog.linear 0 100)

largeNum :: Gen Int
largeNum = Hog.int Hog.linearBounded

wireSize :: Gen Int
wireSize = Hog.int $ Hog.linear 2 200

range :: Gen Range
range = Range <$> fmap fromIntegral wireSize <*> pure 0

genBitVec :: Gen BitVec
genBitVec = BitVec <$> wireSize <*> fmap fromIntegral largeNum

binOp :: Gen BinaryOperator
binOp = Hog.element
    [ BinPlus
    , BinMinus
    , BinTimes
        -- , BinDiv
        -- , BinMod
    , BinEq
    , BinNEq
        -- , BinCEq
        -- , BinCNEq
    , BinLAnd
    , BinLOr
    , BinLT
    , BinLEq
    , BinGT
    , BinGEq
    , BinAnd
    , BinOr
    , BinXor
    , BinXNor
    , BinXNorInv
        -- , BinPower
    , BinLSL
    , BinLSR
    , BinASL
    , BinASR
    ]

unOp :: Gen UnaryOperator
unOp = Hog.element
    [ UnPlus
    , UnMinus
    , UnNot
    , UnLNot
    , UnAnd
    , UnNand
    , UnOr
    , UnNor
    , UnXor
    , UnNxor
    , UnNxorInv
    ]

constExprWithContext :: [Parameter] -> ProbExpr -> Hog.Size -> Gen ConstExpr
constExprWithContext ps prob size
    | size == 0 = Hog.frequency
        [ (prob ^. probExprNum, ConstNum <$> genBitVec)
        , ( if null ps then 0 else prob ^. probExprId
          , ParamId . view paramIdent <$> Hog.element ps
          )
        ]
    | size > 0 = Hog.frequency
        [ (prob ^. probExprNum, ConstNum <$> genBitVec)
        , ( if null ps then 0 else prob ^. probExprId
          , ParamId . view paramIdent <$> Hog.element ps
          )
        , (prob ^. probExprUnOp, ConstUnOp <$> unOp <*> subexpr 2)
        , ( prob ^. probExprBinOp
          , ConstBinOp <$> subexpr 2 <*> binOp <*> subexpr 2
          )
        , ( prob ^. probExprCond
          , ConstCond <$> subexpr 3 <*> subexpr 3 <*> subexpr 3
          )
        , (prob ^. probExprConcat, ConstConcat <$> listOf1 (subexpr 8))
        ]
    | otherwise = constExprWithContext ps prob 0
    where subexpr y = constExprWithContext ps prob $ size `div` y

exprSafeList :: ProbExpr -> [(Int, Gen Expr)]
exprSafeList prob = [(prob ^. probExprNum, Number <$> genBitVec)]

exprRecList :: ProbExpr -> (Hog.Size -> Gen Expr) -> [(Int, Gen Expr)]
exprRecList prob subexpr =
    [ (prob ^. probExprNum     , Number <$> genBitVec)
    , (prob ^. probExprConcat  , Concat <$> listOf1 (subexpr 8))
    , (prob ^. probExprUnOp    , UnOp <$> unOp <*> subexpr 2)
    , (prob ^. probExprStr, Str <$> Hog.text (Hog.linear 0 100) Hog.alphaNum)
    , (prob ^. probExprBinOp   , BinOp <$> subexpr 2 <*> binOp <*> subexpr 2)
    , (prob ^. probExprCond    , Cond <$> subexpr 3 <*> subexpr 3 <*> subexpr 3)
    , (prob ^. probExprSigned  , Appl <$> pure "$signed" <*> subexpr 2)
    , (prob ^. probExprUnsigned, Appl <$> pure "$unsigned" <*> subexpr 2)
    ]

exprWithContext :: ProbExpr -> [Identifier] -> Hog.Size -> Gen Expr
exprWithContext prob [] n | n == 0    = Hog.frequency $ exprSafeList prob
                          | n > 0     = Hog.frequency $ exprRecList prob subexpr
                          | otherwise = exprWithContext prob [] 0
    where subexpr y = exprWithContext prob [] $ n `div` y
exprWithContext prob l n
    | n == 0
    = Hog.frequency
        $ (prob ^. probExprId, Id <$> Hog.element l)
        : exprSafeList prob
    | n > 0
    = Hog.frequency
        $ (prob ^. probExprId, Id <$> Hog.element l)
        : exprRecList prob subexpr
    | otherwise
    = exprWithContext prob l 0
    where subexpr y = exprWithContext prob l $ n `div` y

some :: StateGen a -> StateGen [a]
some f = do
    amount <- gen $ Hog.int (Hog.linear 1 100)
    replicateM amount f

many :: StateGen a -> StateGen [a]
many f = do
    amount <- gen $ Hog.int (Hog.linear 0 100)
    replicateM amount f

makeIdentifier :: T.Text -> StateGen Identifier
makeIdentifier prefix = do
    context <- get
    let ident = Identifier $ prefix <> showT (context ^. nameCounter)
    nameCounter += 1
    return ident

newPort :: PortType -> StateGen Port
newPort pt = do
    ident <- makeIdentifier . T.toLower $ showT pt
    p     <- gen $ Port pt <$> Hog.bool <*> range <*> pure ident
    variables %= (p :)
    return p

scopedExpr :: StateGen Expr
scopedExpr = do
    context <- get
    prob    <- askProbability
    gen . Hog.sized . exprWithContext (prob ^. probExpr) $ vars context
  where
    vars cont =
        (cont ^.. variables . traverse . portName)
            <> (cont ^.. parameters . traverse . paramIdent)

contAssign :: StateGen ContAssign
contAssign = do
    expr <- scopedExpr
    p    <- newPort Wire
    return $ ContAssign (p ^. portName) expr

lvalFromPort :: Port -> LVal
lvalFromPort (Port _ _ _ i) = RegId i

probability :: Config -> Probability
probability c = c ^. configProbability

askProbability :: StateGen Probability
askProbability = lift $ asks probability

assignment :: StateGen Assign
assignment = do
    expr <- scopedExpr
    lval <- lvalFromPort <$> newPort Reg
    return $ Assign lval Nothing expr

seqBlock :: StateGen Statement
seqBlock = do
    stmntDepth -= 1
    tstat <- SeqBlock <$> some statement
    stmntDepth += 1
    return tstat

conditional :: StateGen Statement
conditional = do
    expr  <- scopedExpr
    tstat <- seqBlock
    fstat <- Hog.maybe seqBlock
    return $ CondStmnt expr (Just tstat) fstat

--constToExpr :: ConstExpr -> Expr
--constToExpr (ConstNum s n    ) = Number s n
--constToExpr (ParamId     i   ) = Id i
--constToExpr (ConstConcat c   ) = Concat $ constToExpr <$> c
--constToExpr (ConstUnOp u p   ) = UnOp u (constToExpr p)
--constToExpr (ConstBinOp a b c) = BinOp (constToExpr a) b (constToExpr c)
--constToExpr (ConstCond a b c) =
--    Cond (constToExpr a) (constToExpr b) (constToExpr c)
--constToExpr (ConstStr s) = Str s

forLoop :: StateGen Statement
forLoop = do
    num   <- Hog.int (Hog.linear 0 20)
    var   <- lvalFromPort <$> newPort Reg
    stats <- seqBlock
    return $ ForLoop (Assign var Nothing 0)
                     (BinOp (varId var) BinLT $ fromIntegral num)
                     (Assign var Nothing $ BinOp (varId var) BinPlus 1)
                     stats
    where varId v = Id (v ^. regId)

statement :: StateGen Statement
statement = do
    prob <- askProbability
    cont <- get
    let defProb i = prob ^. probStmnt . i
    Hog.frequency
        [ (defProb probStmntBlock              , BlockAssign <$> assignment)
        , (defProb probStmntNonBlock           , NonBlockAssign <$> assignment)
        , (onDepth cont (defProb probStmntCond), conditional)
        , (onDepth cont (defProb probStmntFor) , forLoop)
        ]
    where onDepth c n = if c ^. stmntDepth > 0 then n else 0

always :: StateGen ModItem
always = do
    stat <- SeqBlock <$> some statement
    return $ Always (EventCtrl (EPosEdge "clk") (Just stat))

instantiate :: ModDecl -> StateGen ModItem
instantiate (ModDecl i outP inP _ _) = do
    context <- get
    outs    <-
        fmap (Id . view portName) <$> (replicateM (length outP) $ newPort Wire)
    ins <-
        (Id "clk" :)
        .   fmap (Id . view portName)
        .   take (length inP - 1)
        <$> (Hog.shuffle $ context ^. variables)
    ident <- makeIdentifier "modinst"
    Hog.choice
        [ return . ModInst i ident $ ModConn <$> outs <> ins
        , ModInst i ident <$> Hog.shuffle
            (zipWith ModConnNamed (view portName <$> outP <> inP) (outs <> ins))
        ]

-- | Generates a module instance by also generating a new module if there are
-- not enough modules currently in the context. It keeps generating new modules
-- for every instance and for every level until either the deepest level is
-- achieved, or the maximum number of modules are reached.
--
-- If the maximum number of levels are reached, it will always pick an instance
-- from the current context. The problem with this approach is that at the end
-- there may be many more than the max amount of modules, as the modules are
-- always set to empty when entering a new level. This is to fix recursive
-- definitions of modules, which are not defined.
--
-- One way to fix that is to also decrement the max modules for every level,
-- depending on how many modules have already been generated. This would mean
-- there would be moments when the module cannot generate a new instance but
-- also not take a module from the current context. A fix for that may be to
-- have a default definition of a simple module that is used instead.
--
-- Another different way to handle this would be to have a probability of taking
-- a module from a context or generating a new one.
modInst :: StateGen ModItem
modInst = do
    prob    <- lift ask
    context <- get
    let maxMods = prob ^. configProperty . propMaxModules
    if length (context ^. modules) < maxMods
        then do
            let currMods = context ^. modules
            let params   = context ^. parameters
            let vars     = context ^. variables
            modules .= []
            variables .= []
            parameters .= []
            modDepth -= 1
            chosenMod <- moduleDef Nothing
            ncont     <- get
            let genMods = ncont ^. modules
            modDepth += 1
            parameters .= params
            variables .= vars
            modules .= chosenMod : currMods <> genMods
            instantiate chosenMod
        else Hog.element (context ^. modules) >>= instantiate

-- | Generate a random module item.
modItem :: StateGen ModItem
modItem = do
    prob    <- askProbability
    context <- get
    let defProb i = prob ^. probModItem . i
    Hog.frequency
        [ (defProb probModItemAssign, ModCA <$> contAssign)
        , (defProb probModItemAlways, always)
        , ( if context ^. modDepth > 0 then defProb probModItemInst else 0
          , modInst
          )
        ]

moduleName :: Maybe Identifier -> StateGen Identifier
moduleName (Just t) = return t
moduleName Nothing  = makeIdentifier "module"

constExpr :: StateGen ConstExpr
constExpr = do
    prob    <- askProbability
    context <- get
    gen . Hog.sized $ constExprWithContext (context ^. parameters)
                                           (prob ^. probExpr)

parameter :: StateGen Parameter
parameter = do
    ident <- makeIdentifier "param"
    cexpr <- constExpr
    let param = Parameter ident cexpr
    parameters %= (param :)
    return param

-- | Generates a module definition randomly. It always has one output port which
-- is set to @y@. The size of @y@ is the total combination of all the locally
-- defined wires, so that it correctly reflects the internal state of the
-- module.
moduleDef :: Maybe Identifier -> StateGen ModDecl
moduleDef top = do
    name     <- moduleName top
    portList <- some $ newPort Wire
    mi       <- Hog.list (Hog.linear 4 100) modItem
    context  <- get
    let local = filter (`notElem` portList) $ context ^. variables
    let size  = sum $ local ^.. traverse . portSize
    let clock = Port Wire False 1 "clk"
    let yport = Port Wire False size "y"
    let comb  = combineAssigns_ yport local
    declareMod local
        .   ModDecl name [yport] (clock : portList) (mi <> [comb])
        <$> many parameter

-- | Procedural generation method for random Verilog. Uses internal 'Reader' and
-- 'State' to keep track of the current Verilog code structure.
procedural :: Config -> Gen Verilog
procedural config = do
    (mainMod, st) <- Hog.resize num
        $ runReaderT (runStateT (moduleDef (Just "top")) context) config
    return . Verilog $ mainMod : st ^. modules
  where
    context =
        Context [] [] [] 0 (confProp propStmntDepth) $ confProp propModDepth
    num = fromIntegral $ confProp propSize
    confProp i = config ^. configProperty . i

proceduralIO :: Config -> IO Verilog
proceduralIO = Hog.sample . procedural
