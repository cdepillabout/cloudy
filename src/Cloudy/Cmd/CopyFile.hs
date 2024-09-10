{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.CopyFile where

import Cloudy.Cli (CopyFileCliOpts (..), CopyFileDirection (..), Recursive (..))
import Cloudy.Cmd.Utils (SelectInstBy, findInstanceInfoForSelectInstBy, mkSelectInstBy)
import Cloudy.LocalConfFile (LocalConfFileOpts (..))
import Cloudy.Db (withCloudyDb, InstanceInfo (..), ScalewayInstance (..))
import Data.Text (unpack, Text)
import Data.Void (absurd)
import System.Posix.Process (executeFile)
import Control.Monad (when)
import Control.FromSum (fromMaybeM)

data CopyFileSettings = CopyFileSettings
  { selectInstBy :: SelectInstBy
  , direction :: CopyFileDirection
  , recursive :: Recursive
  , filesToCopyArgs :: [Text]
  }
  deriving stock Show

mkSettings :: LocalConfFileOpts -> CopyFileCliOpts -> IO CopyFileSettings
mkSettings _localConfFileOpts cliOpts = do
  selectInstBy <- mkSelectInstBy cliOpts.id cliOpts.name
  pure
    CopyFileSettings
      { selectInstBy
      , direction = cliOpts.direction
      , recursive = cliOpts.recursive
      , filesToCopyArgs = cliOpts.filesToCopyArgs
      }

runCopyFile :: LocalConfFileOpts -> CopyFileCliOpts -> IO ()
runCopyFile localConfFileOpts cliOpts = do
  settings <- mkSettings localConfFileOpts cliOpts
  ipAddr <- withCloudyDb $ \conn -> do
    instanceInfo <- findInstanceInfoForSelectInstBy conn settings.selectInstBy
    case instanceInfo of
      CloudyAwsInstance _cloudyInstance void -> absurd void
      CloudyScalewayInstance _cloudyInstance scalewayInstance -> pure scalewayInstance.scalewayIpAddress
  when (length settings.filesToCopyArgs <= 1) $
    error "ERROR: must pass at least 2 files"
  (firstFiles, lastFile) <-
    fromMaybeM
      (error "ERROR: unsnoc should have succeeded since we check there are at least 2 files")
      (unsnoc settings.filesToCopyArgs)
  let recursiveArg =
        case settings.recursive of
          Recursive -> ["-r"]
          NoRecursive -> []
      scpFilesArgs =
        case settings.direction of
          FromInstanceToLocal ->
            fmap (\file -> "root@" <> ipAddr <> ":" <> file) firstFiles <>
            [lastFile]
          ToInstanceFromLocal -> firstFiles <> ["root@" <> ipAddr <> ":" <> lastFile]
      scpArgs =
        fmap unpack $ recursiveArg <> scpFilesArgs
  putStrLn $ "About to run scp command: " <> show ("scp" : scpArgs)
  executeFile "scp" True scpArgs Nothing

-- TODO: Available from Data.List in GHC-9.8
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
