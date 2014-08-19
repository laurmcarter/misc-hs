
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.List

import System.Environment
import System.FilePath.Posix
import System.IO
import System.IO.Error
import System.Process

musicDir = "MUSIC_DIR"
currentFile = "MPLAYER_CURRENT"

type Artist = String
type Album  = String

data Track = Track
  { trackArtist :: Artist
  , trackAlbum  :: Album
  , trackName   :: String
  } deriving (Show)

main :: IO ()
main = do
  ts <- getArgs
  dir <- return $ Just "/home/kcarter/Music" -- tryGetEnv musicDir
  whenJust dir $ \d -> do
    mp <- playFiles d ts
    forkIO $ forever $ do
      l <- hGetLine $ mpOut mp
      putStr l
    forever $ threadDelay 120000000

--------------------------------------------------------------------------------

tryGetEnv :: String -> IO (Maybe String)
tryGetEnv s = do
  me <- tryIOError $ getEnv s
  case me of
    Left err  | isDoesNotExistError err -> return Nothing
              | otherwise               -> ioError err
    Right res                           -> return $ Just res

data Mplayer = Mplayer
  { mpIn   :: Handle
  , mpOut  :: Handle
  , mpErr  :: Handle
  , mpProc :: ProcessHandle
  , mpDir  :: FilePath
  }

playFiles :: FilePath -> [String] {- [Track] -} -> IO Mplayer
playFiles dir s = do
  (i,o,e,pid) <- runInteractiveProcess "mplayer" s Nothing Nothing
  return (Mplayer i o e pid dir)
  --where
  --fs = map (formatTrackGlob dir) ts

fmt :: String -> FilePath
fmt = map $ \c -> if c == ' ' then '_' else c

parseTrack :: FilePath -> Maybe Track
parseTrack fp = case take 3 $ reverse $ splitPath fp of
  [name,album,artist] -> Just $ Track (unFmt artist) (unFmt album) (unFmtName name)
  _ -> Nothing
  where
  unFmtName n = if not (null n) && isDigit (head n)
    then unFmt $ drop 1 $ snd $ break (== '_') n
    else unFmt n

unFmt :: FilePath -> String
unFmt = dropExtension . dropTrailingPathSeparator . map (\c -> if c == '_' then ' ' else c)

formatTrackGlob :: FilePath -> Track -> FilePath
formatTrackGlob dir (Track artist album name) =
  dir </> fmt artist </> fmt album </> ("*" ++ fmt name ++ "*")

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
  Nothing -> return ()
  Just a  -> f a

