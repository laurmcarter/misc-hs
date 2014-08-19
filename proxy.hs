
import Control.Monad
import Control.Proxy
import System.IO

lines' :: (Proxy p) => Handle -> () -> Producer p String IO ()
lines' h () = runIdentityP loop where
  loop = do
    eof <- lift $ hIsEOF h
    if eof
    then return ()
    else do
      str <- lift $ hGetLine h
      respond str
      loop

promptInt :: (Proxy p) => () -> Producer p Int IO r
promptInt () = runIdentityP $ forever $ do
  lift $ putStrLn "Enter an integer:"
  n <- lift readLn
  respond n

promptInt2 :: (Proxy p) => () -> Producer p Int [] r
promptInt2 () = runIdentityP $ forever $ respond 1

printer :: (Proxy p, Show a) => () -> Consumer p a IO r
printer () = runIdentityP $ forever $ do
  a <- request ()
  lift $ do
    putStrLn "Received a value:"
    print a

printer2 :: (Proxy p, Show a) => () -> Consumer p a [] r
printer2 () = runIdentityP $ forever $ do
  a <- request ()
  return a

take'2 :: (Proxy p) => Int -> () -> Pipe p a a [] ()
take'2 n () = runIdentityP $ do
  replicateM_ n $ do
    a <- request ()
    respond a

take' :: (Proxy p) => Int -> () -> Pipe p a a IO ()
take' n () = runIdentityP $ do
  replicateM_ n $ do
    a <- request ()
    respond a
  lift $ putStrLn "You shall not pass!"

