-- Module      : Verismith.Verilog2005.Eval
-- Description : Evaluation of Verilog expressions.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

{-# LANGUAGE OverloadedLists #-}

module Verismith.Verilog2005.Eval
  ( Value (..),
--    evalConstExprSelfDetermined
  )
where

import qualified Data.HashMap.Strict as HashMap
import Numeric.Natural
import Data.Bits
import GHC.Float
import qualified Numeric.Extras as NExt
import Text.Printf (printf)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.List (foldl')
import qualified Data.ByteString as BS
import Data.ByteString.Internal (unpackChars, c2w, w2c)
import Data.List.NonEmpty (NonEmpty (..), (<|), toList)
import qualified Data.List.NonEmpty as NE
import Verismith.Verilog2005.AST
import Verismith.Verilog2005.ElabAST
import Verismith.Utils (nonEmpty, foldrMap1, uncurry3, nTimes)

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

instance Show Value where
  show x = case x of
    VInt sn sz v m ->
      show
        ( PrimNumber
            sz
            sn
            (NBinary $ case red v m [] of [] -> [BXZ0]; h : t -> h :| t)
            :: GenPrim () () ()
        )
    VReal r -> show r
    VNothing -> "{}"
    where
      red x y acc =
        let
          nx = x .<<. 1
          ny = y .<<. 1
         in if x == 0 && y == 0
              then acc
              else
                red nx ny . (: acc) $ case (testBit x 0, testBit y 0) of
                  (False, False) -> BXZ0
                  (True, False) -> BXZ1
                  (False, True) -> BXZZ
                  (True, True) -> BXZX


getConstExprSize :: GenExpr i r a -> Maybe (Maybe Natural)
getConstExprSize x = case x of

-- TODO HERE: size and SIGN
getConstPrimSize :: GenPrim i r a -> Maybe (Maybe (Bool, Natural))
getConstPrimSize x = case x of
  PrimNumber sz _ _ -> Just $ Just sz
  PrimReal _ -> Just $ Nothing
  PrimIdent _ _ -> Nothing
  PrimConcat l -> Just . sum <$> traverse (fmap fromJust . getConstExprSize) l
  PrimMultConcat n l -> do
    mult <- evalConstExprSelfDetermined n
    Just . (mult *) . sum <$> traverse (fmap fromJust . getConstExprSize)
  PrimFun _ _ _ -> Nothing
  PrimSysFun i args ->
    traverse getConstExprSize args >>= \l -> case HashMap.lookup i sfMap of
      Just SFUnsigned -> case l of
        [Just sz] -> Just $ Just sz
        _ -> error "Invalid argument to $unsigned"
      Just SFSigned -> case l of
        [Just sz] -> Just $ Just sz
        _ -> error "Invalid argument to $signed"
      Just SFRealtobits -> case l of -- Signedness is not specified in the standard
        [Nothing] -> Just $ Just 64
        _ -> error "Invalid argument to $realtobits"
      Just SFBitstoreal -> case l of -- Signedness is not specified in the standard
        [VInt _ 64 v 0] -> Just $ VReal $ castWord64ToDouble $ fromInteger v
        _ -> error "Invalid argument to $bitstoreal"
      Just SFRtoi -> case l of
        [VReal r] | isDoubleFinite r /= 0 -> Just $ VInt True 0 (truncate r) 0
        [VReal r] -> Nothing -- Idk what I'm supposed to return
        _ -> error "Invalid argument to $rtoi"
      Just SFItor -> case l of
        [VInt sn sz v 0] -> Nothing -- too complicated and I don't know what to do outside of range...
        [VInt sn sz v m] -> Nothing -- TODO:"x and z are considered 0", round to nearest away from 0
        _ -> error "Invalid argument to $itor"
      Just SFClog2 -> case l of -- Is sign following usual expression rules?
        [VInt sn sz v 0] ->
          Just $ VInt sn 0 (toInteger $ locateFirstBit (fitSnSz False sz v) $ fromEnum sz) 0
        [VInt _ _ _ m] -> Nothing
        _ -> error "Invalid argument to $clog2"
      Just SFLn -> case l of [VReal r] -> Just $ VReal $ log r; _ -> error "Invalid argument to $ln"
      Just SFLog10 ->
        case l of [VReal r] -> Just $ VReal $ logBase 10 r; _ -> error "Invalid argument to $log10"
      Just SFExp ->
        case l of [VReal r] -> Just $ VReal $ exp r; _ -> error "Invalid argument to $exp"
      Just SFSqrt ->
        case l of [VReal r] -> Just $ VReal $ sqrt r; _ -> error "Invalid argument to $sqrt"
      Just SFPow -> case l of
        [VReal r0, VReal r1] -> Just $ VReal $ r0 ** r1
        _ -> error "Invalid argument to $pow"
      Just SFFloor ->
        case l of [VReal r] -> Just $ VReal $ NExt.floor r; _ -> error "Invalid argument to $floor"
      Just SFCeil ->
        case l of [VReal r] -> Just $ VReal $ NExt.ceil r; _ -> error "Invalid argument to $ceil"
      Just SFSin ->
        case l of [VReal r] -> Just $ VReal $ sin r; _ -> error "Invalid argument to $sin"
      Just SFCos ->
        case l of [VReal r] -> Just $ VReal $ cos r; _ -> error "Invalid argument to $cos"
      Just SFTan ->
        case l of [VReal r] -> Just $ VReal $ tan r; _ -> error "Invalid argument to $tan"
      Just SFAsin ->
        case l of [VReal r] -> Just $ VReal $ asin r; _ -> error "Invalid argument to $asin"
      Just SFAcos ->
        case l of [VReal r] -> Just $ VReal $ acos r; _ -> error "Invalid argument to $acos"
      Just SFAtan ->
        case l of [VReal r] -> Just $ VReal $ atan r; _ -> error "Invalid argument to $atan"
      Just SFAtan2 -> case l of
        [VReal r0, VReal r1] -> Just $ VReal $ atan2 r0 r1
        _ -> error "Invalid argument to $atan2"
      Just SFHypot -> case l of
        [VReal r0, VReal r1] -> Just $ VReal $ NExt.hypot r0 r1
        _ -> error "Invalid argument to $hypot"
      Just SFSinh ->
        case l of [VReal r] -> Just $ VReal $ sinh r; _ -> error "Invalid argument to $sinh"
      Just SFCosh ->
        case l of [VReal r] -> Just $ VReal $ cosh r; _ -> error "Invalid argument to $cosh"
      Just SFTanh ->
        case l of [VReal r] -> Just $ VReal $ tanh r; _ -> error "Invalid argument to $tanh"
      Just SFAsinh ->
        case l of [VReal r] -> Just $ VReal $ asinh r; _ -> error "Invalid argument to $asinh"
      Just SFAcosh ->
        case l of [VReal r] -> Just $ VReal $ acosh r; _ -> error "Invalid argument to $acosh"
      Just SFAtanh ->
        case l of [VReal r] -> Just $ VReal $ atanh r; _ -> error "Invalid argument to $atanh"
      Just SFRandom ->
        case l of [] -> Nothing; [VInt _ _ _ _] -> Nothing; _ -> error "Invalid argument to $random"
      Just SFDisterlang -> case l of
        [VInt _ _ _ _, VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_erlang"
      Just SFDistnormal -> case l of
        [VInt _ _ _ _, VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_normal"
      Just SFDistt ->
        case l of [VInt _ _ _ _, VInt _ _ _ _] -> Nothing; _ -> error "Invalid argument to $dist_t"
      Just SFDistchisquare -> case l of
        [VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_chi_square"
      Just SFDistexponential -> case l of
        [VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_exponential"
      Just SFDistpoisson -> case l of
        [VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_poisson"
      Just SFDistuniform -> case l of
        [VInt _ _ _ _, VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_uniform"
      _ -> error "Invalid system function"
  PrimMinTypMax (MTMSingle e) -> getConstExprSize e
  PrimMinTypMax mtm -> Nothing
  PrimString s -> Just $ Just $ 8 * toEnum (max 1 $ BS.length s)

evalConstExprSize :: GenExpr i r a -> Maybe (Maybe Natural, Either Value (GenExpr i r a))
evalConstExprSize x = case x of

evalConstPrimSize :: GenPrim i r a -> Maybe (Maybe Natural, Either Value (GenPrim i r a))
evalConstPrimSize x = case x of
  PrimNumber sz sn v -> Just $ evalNumber sz sn v
  PrimReal s -> Just $ VReal $ read $ unpackChars s
  PrimIdent _ _ -> Nothing
  PrimConcat l -> uncurry3 (VInt False) <$> evalCat l
  PrimMultConcat n l -> evalConstExprSelfDetermined n >>= \mult -> case mult of
    VInt sn sz v 0 ->
      let tval = fitSnSz sn sz v
       in case compare tval 0 of
            LT -> error "Negative replication is not allowed"
            EQ -> Just VNothing
            GT -> do
              (sz, v, m) <- evalCat l
              let isz = fromEnum sz
              return $
                VInt
                  False
                  (sz * fromInteger tval)
                  (nTimes (\x -> (x .<<. isz) .|. v) 0 tval)
                  (nTimes (\x -> (x .<<. isz) .|. m) 0 tval)
    _ -> error "Invalid multiplicity for replication"
  PrimFun _ _ _ -> Nothing
  PrimSysFun i args ->
    traverse evalConstExprSelfDetermined args >>= \l -> case HashMap.lookup i sfMap of
      Just SFUnsigned -> case l of
        [VInt sn sz v m] -> Just $ VInt False sz v m
        _ -> error "Invalid argument to $unsigned"
      Just SFSigned -> case l of
        [VInt sn sz v m] -> Just $ VInt True sz v m
        _ -> error "Invalid argument to $signed"
      Just SFRealtobits -> case l of -- Signedness is not specified in the standard
        [VReal r] -> Just $ VInt False 64 (toInteger $ castDoubleToWord64 r) 0
        _ -> error "Invalid argument to $realtobits"
      Just SFBitstoreal -> case l of -- Signedness is not specified in the standard
        [VInt _ 64 v 0] -> Just $ VReal $ castWord64ToDouble $ fromInteger v
        _ -> error "Invalid argument to $bitstoreal"
      Just SFRtoi -> case l of
        [VReal r] | isDoubleFinite r /= 0 -> Just $ VInt True 0 (truncate r) 0
        [VReal r] -> Nothing -- Idk what I'm supposed to return
        _ -> error "Invalid argument to $rtoi"
      Just SFItor -> case l of
        [VInt sn sz v 0] -> Nothing -- too complicated and I don't know what to do outside of range...
        [VInt sn sz v m] -> Nothing -- TODO:"x and z are considered 0", round to nearest away from 0
        _ -> error "Invalid argument to $itor"
      Just SFClog2 -> case l of -- Is sign following usual expression rules?
        [VInt sn sz v 0] ->
          Just $ VInt sn 0 (toInteger $ locateFirstBit (fitSnSz False sz v) $ fromEnum sz) 0
        [VInt _ _ _ m] -> Nothing
        _ -> error "Invalid argument to $clog2"
      Just SFLn -> case l of [VReal r] -> Just $ VReal $ log r; _ -> error "Invalid argument to $ln"
      Just SFLog10 ->
        case l of [VReal r] -> Just $ VReal $ logBase 10 r; _ -> error "Invalid argument to $log10"
      Just SFExp ->
        case l of [VReal r] -> Just $ VReal $ exp r; _ -> error "Invalid argument to $exp"
      Just SFSqrt ->
        case l of [VReal r] -> Just $ VReal $ sqrt r; _ -> error "Invalid argument to $sqrt"
      Just SFPow -> case l of
        [VReal r0, VReal r1] -> Just $ VReal $ r0 ** r1
        _ -> error "Invalid argument to $pow"
      Just SFFloor ->
        case l of [VReal r] -> Just $ VReal $ NExt.floor r; _ -> error "Invalid argument to $floor"
      Just SFCeil ->
        case l of [VReal r] -> Just $ VReal $ NExt.ceil r; _ -> error "Invalid argument to $ceil"
      Just SFSin ->
        case l of [VReal r] -> Just $ VReal $ sin r; _ -> error "Invalid argument to $sin"
      Just SFCos ->
        case l of [VReal r] -> Just $ VReal $ cos r; _ -> error "Invalid argument to $cos"
      Just SFTan ->
        case l of [VReal r] -> Just $ VReal $ tan r; _ -> error "Invalid argument to $tan"
      Just SFAsin ->
        case l of [VReal r] -> Just $ VReal $ asin r; _ -> error "Invalid argument to $asin"
      Just SFAcos ->
        case l of [VReal r] -> Just $ VReal $ acos r; _ -> error "Invalid argument to $acos"
      Just SFAtan ->
        case l of [VReal r] -> Just $ VReal $ atan r; _ -> error "Invalid argument to $atan"
      Just SFAtan2 -> case l of
        [VReal r0, VReal r1] -> Just $ VReal $ atan2 r0 r1
        _ -> error "Invalid argument to $atan2"
      Just SFHypot -> case l of
        [VReal r0, VReal r1] -> Just $ VReal $ NExt.hypot r0 r1
        _ -> error "Invalid argument to $hypot"
      Just SFSinh ->
        case l of [VReal r] -> Just $ VReal $ sinh r; _ -> error "Invalid argument to $sinh"
      Just SFCosh ->
        case l of [VReal r] -> Just $ VReal $ cosh r; _ -> error "Invalid argument to $cosh"
      Just SFTanh ->
        case l of [VReal r] -> Just $ VReal $ tanh r; _ -> error "Invalid argument to $tanh"
      Just SFAsinh ->
        case l of [VReal r] -> Just $ VReal $ asinh r; _ -> error "Invalid argument to $asinh"
      Just SFAcosh ->
        case l of [VReal r] -> Just $ VReal $ acosh r; _ -> error "Invalid argument to $acosh"
      Just SFAtanh ->
        case l of [VReal r] -> Just $ VReal $ atanh r; _ -> error "Invalid argument to $atanh"
      Just SFRandom ->
        case l of [] -> Nothing; [VInt _ _ _ _] -> Nothing; _ -> error "Invalid argument to $random"
      Just SFDisterlang -> case l of
        [VInt _ _ _ _, VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_erlang"
      Just SFDistnormal -> case l of
        [VInt _ _ _ _, VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_normal"
      Just SFDistt ->
        case l of [VInt _ _ _ _, VInt _ _ _ _] -> Nothing; _ -> error "Invalid argument to $dist_t"
      Just SFDistchisquare -> case l of
        [VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_chi_square"
      Just SFDistexponential -> case l of
        [VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_exponential"
      Just SFDistpoisson -> case l of
        [VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_poisson"
      Just SFDistuniform -> case l of
        [VInt _ _ _ _, VInt _ _ _ _, VInt _ _ _ _] -> Nothing
        _ -> error "Invalid argument to $dist_uniform"
      _ -> error "Invalid system function"
  PrimMinTypMax (MTMSingle e) -> evalConstExprSelfDetermined e
  PrimMinTypMax mtm -> Nothing
  PrimString s -> Just $ evalString s
