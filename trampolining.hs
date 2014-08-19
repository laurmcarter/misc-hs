{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Control.Lens
import Control.Monad.State

import Debug.Trace

data I = I !Int deriving (Eq,Ord,Show)

instance Num I where
  fromInteger = I . fromInteger
  (I x) + (I y) = I (x + y)
  (I x) - (I y) = I (x - y)
  (I x) * (I y) = I (x * y)
  abs (I x)     = I (abs x)
  signum (I x)  = I (signum x)

factorial :: I -> I
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial_cps :: I -> (I -> I) -> I
factorial_cps 0 k = k 1
factorial_cps n k = factorial_cps (n - 1) (k . (* n))

data K
  = MT
  | K !I !K

{-
applyK :: K -> I -> I
applyK MT n = n
applyK (K m k) n = applyK k (m * n)

factorial_driver :: I -> I
factorial_driver n = factorial_cps_defun n MT

factorial_cps_defun :: I -> K -> I
factorial_cps_defun 0 k = applyK k 1
factorial_cps_defun n k = factorial_cps_defun (n - 1) (K n k)
-}

data Regs = Regs
  { _nReg    :: I
  , _kReg    :: K
  , _vReg    :: I
  , _pcReg   :: M ()
  , _doneReg :: Bool
  }

type M = State Regs

makeLenses ''Regs

initRegs :: Regs
initRegs = Regs
  undefined
  undefined
  undefined
  undefined
  undefined

factorial_reg :: I -> I
factorial_reg n = evalState go initRegs
  where
  go = do
    nReg    .= n
    kReg    .= MT
    pcReg   .= factorial_reg_cps
    doneReg .= False
    -- liftIO $ putStrLn "mount"
    trampoline

factorial_reg_cps :: M ()
factorial_reg_cps = do
  n <- use nReg
  if n <= 0
    then do
      vReg .= 1
      pcReg .= applyK_reg
    else do
      kReg %= K n
      nReg -= 1
      pcReg .= factorial_reg_cps

applyK_reg :: M ()
applyK_reg = do
  k <- use kReg
  case k of
    MT -> doneReg .= True
    K n k' -> do
      kReg .= k'
      vReg %= (* n)
      pcReg .= applyK_reg

bounce :: M ()
bounce = join $ use pcReg

trampoline :: M I
trampoline = do
  done <- use doneReg
  if done
    then do
      -- liftIO $ putStrLn "dismount"
      use vReg
    else do
      -- liftIO $ putStrLn "bounce"
      bounce
      trampoline

