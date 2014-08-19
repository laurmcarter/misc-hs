
import System.Console.Haskeline

import Control.Monad
import Control.Monad.Trans.Class

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop :: InputT IO ()
  loop = do
    mi <- getInputLine "% "
    case mi of
      Nothing        -> return ()
      Just (':':cmd) -> handleCmd loop cmd
      Just input     -> outputStrLn ("Input was: " ++ input) >> loop

handleCmd :: InputT IO () -> String -> InputT IO ()
handleCmd loop s = case s of
  "q" -> return ()
  "h" -> halp >> loop
  _   -> outputStrLn ("Unrecognized command: " ++ s) >> loop

halp :: InputT IO ()
halp = do
  outputStrLn "You asked for halp."
  outputStrLn "You get none."

runCmd :: (String -> IO ()) -> IO ()
runCmd c = runInputT defaultSettings loop
  where
  loop :: InputT IO ()
  loop = do
    mi <- getInputLine "% "
    case mi of
      Nothing        -> return ()
      Just (':':cmd) -> handleCmd loop cmd
      Just input     -> lift (c input) >> loop

