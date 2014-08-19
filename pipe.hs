
import Control.Monad
import Control.Proxy
import System.IO

import qualified Data.Map as M

take' :: (Proxy p) => Int -> () -> Pipe p a a IO ()
take' n () = runIdentityP $ do
  replicateM_ n (request () >>= respond)
  lift $ putStrLn "That's all we need."

promptInt :: (Proxy p) => () -> Producer p Int IO r
promptInt () = runIdentityP $ forever $ do
  lift $ putStrLn "Enter an Integer:"
  n <- lift readLn
  respond n

printer :: (Proxy p, Show a) => () -> Consumer p a IO r
printer () = runIdentityP $ forever $ do
  a <- request ()
  lift $ print a

threeReqs :: (Proxy p) => () -> Client p Int Bool IO ()
threeReqs () = runIdentityP $ forM_ [1,3,1] $ \arg -> do
  lift $ putStrLn ("Client Sends: " ++ show (arg :: Int))
  result <- request arg
  lift $ putStrLn ("Client Receives: " ++ show (result :: Bool))
  lift $ putStrLn "*"

comparer :: (Proxy p) => Int -> Server p Int Bool IO r
comparer = runIdentityK loop
  where
  loop arg = do
    lift $ putStrLn ("Server Receives: " ++ show (arg :: Int))
    let result = arg > 2
    lift $ putStrLn ("Server Sends: " ++ show (result :: Bool))
    nextArg <- respond result
    loop nextArg

cache :: (Proxy p, Ord key) => key -> p key val key val IO r
cache = runIdentityK (loop M.empty)
  where
  loop m key = case M.lookup key m of
    Nothing -> do
      val <- request key
      key2 <- respond val
      loop (M.insert key val m) key2
    Just val -> do
      lift $ putStrLn "Used cache!"
      key2 <- respond val
      loop m key2

name :: Proxy p => () -> Producer p String IO r
name () = runIdentityP $ forever $ do
  lift $ putStrLn "What's your name?"
  nm <- lift getLine
  respond nm

lines' :: Proxy p => () -> Pipe p Char String IO ()
lines' () = runIdentityP (loop [])
  where
  loop l = do
    c <- request ()
    case c of
      '\n' -> do
        respond l
        loop []
      _ -> do
        loop (l ++ [c])

bytes :: Proxy p => Handle -> () -> Producer p Char IO ()
bytes h () = runIdentityP loop
  where
  loop = do
    eof <- lift $ hIsEOF h
    if eof
    then return ()
    else do
      c <- lift $ hGetChar h
      respond c
      loop

trickle :: Proxy p => () -> Pipe p String Char IO ()
trickle () = runIdentityP loop
  where
  loop = do
    s <- request ()
    mapM_ respond s
    loop

chunker :: Proxy p => Int -> () -> Pipe p Char String IO ()
chunker size () = runIdentityP (loop [])
  where
  loop l = do
    c <- request ()
    let l' = l ++ [c]
    if length l' == size
    then do
      respond l'
      loop []
    else do
      loop l'

