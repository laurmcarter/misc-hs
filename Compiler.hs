{-# LANGUAGE RebindableSyntax #-}

import IMonad
-- import qualified Err as E

import ContAbort

import Prelude hiding ((>>=), (>>), return, fail)
import qualified Prelude as Pr ((>>=), (>>), return, fail)
import Control.Arrow hiding (loop)
import Control.Monad ((>=>))
import Data.List

type Err = ContAbort String (IO ())

runE :: Err a -> (String -> IO ()) -> (a -> IO ()) -> IO ()
runE = runContAbort

type CompilerRes = IWriter (Kleisli Err)
type Compiler a b = CompilerRes a b ()

runCompiler :: (Show a, Show b) => Compiler a b -> a -> IO ()
runCompiler c a = let e = runKleisli (execIWriter c) a in
  runE e
    (putStrLn . ("Error\n" ++) . indent 2)  -- failure
    (putStrLn . indent 2 . show)            -- success

type Pass a b = a -> Err b

pass :: (Show a, Show b) => Pass a b -> String -> Bool -> Compiler a b
pass p name trace = mkCompiler $ \input -> handle (p input)
    (\k err    -> k $ intercalate "\n" [label err, "in:" ++ show input])
    (\k output -> if trace
      then runNonIxd $ do
        liftNI $ putStrLn (label "")
        liftNI $ putStr $ indent 2 ("input: " ++ show input)
        liftNI $ putStr $ indent 2 ("output: " ++ show output)
        liftNI $ putStrLn ""
        liftNI $ k output
      else k output)
  where
  label s = name ++ ": " ++ s

foo :: Pass Int Bool
foo i
  | i < 5 = Pr.return True
  | True  = Pr.return False

bar :: Pass Bool String
bar = Pr.return . show

baz :: Pass String Int
baz s = let l = length s in
  if l < 5
    then Pr.fail "too short"
    else Pr.return l

quux :: Pass Int Int
quux i = Pr.return (i * 2)

bloz :: Pass Int Int
bloz i = Pr.return (i - 1)

goodEnough :: Int -> Bool
goodEnough n = n > 1000000

compiler :: Compiler Int Int
compiler = do
  pass foo "foo" True
  pass bar "bar" True
  pass baz "baz" True
  doUntil goodEnough "goodEnough" True $ do
    pass quux "quux" True
    pass bloz "bloz" True

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

mkCompiler :: Pass a b -> Compiler a b
mkCompiler = tell . Kleisli

