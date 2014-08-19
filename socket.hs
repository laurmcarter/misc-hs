
import Network
import System.IO
import Control.Monad
import Control.Concurrent

connect :: IO ()
connect = do
  h <- connectTo "niven.freenode.net" (PortNumber 6667)
  forkIO $ forever (hGetLine h >>= putStrLn)
  forever $ do
    cmd <- getLine
    hPutStrLn h cmd

main = connect

