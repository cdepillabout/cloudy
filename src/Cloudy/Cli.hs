
module Cloudy.Cli
  ( parseCliOpts
  , CliCmd(..)
  , ScalewayCliOpts(..)
  , AwsCliOpts(..)
  , ListCliOpts(..)
  , SshCliOpts(..)
  , CopyFileCliOpts(..)
  , DestroyCliOpts(..)
  , CopyFileDirection(..)
  , Recursive(..)
  )
  where

import Cloudy.Cli.Aws (AwsCliOpts(..), awsCliOptsParser)
import Cloudy.Cli.Scaleway (ScalewayCliOpts(..), scalewayCliOptsParser)
import Cloudy.Db (CloudyInstanceId (..))
import Data.Text (Text)
import Options.Applicative
  ( Alternative((<|>)), Parser, (<**>), command, fullDesc, header, info
  , progDesc, execParser, helper, footer, hsubparser, ParserInfo, strOption, long, short, metavar, help, option, auto, noIntersperse, forwardOptions, strArgument, footerDoc, flag', flag )
import Control.Applicative (Alternative(many))
import Options.Applicative.Help (vsep)

data CliCmd
  = Aws AwsCliOpts
  | List ListCliOpts
  | Scaleway ScalewayCliOpts
  | Ssh SshCliOpts
  | CopyFile CopyFileCliOpts
  | Destroy DestroyCliOpts
  deriving stock Show

data ListCliOpts = ListCliOpts
  deriving stock Show

data SshCliOpts = SshCliOpts
  { id :: Maybe CloudyInstanceId
  , name :: Maybe Text
  , passthru :: [Text]
  }
  deriving stock Show

data CopyFileCliOpts = CopyFileCliOpts
  { id :: Maybe CloudyInstanceId
  , name :: Maybe Text
  , direction :: CopyFileDirection
  , recursive :: Recursive
  , filesToCopyArgs :: [Text]
  }
  deriving stock Show

data DestroyCliOpts = DestroyCliOpts
  { id :: Maybe CloudyInstanceId
  , name :: Maybe Text
  }
  deriving stock Show

-- | Which direction to copy files in the @copy-file@ command
data CopyFileDirection = FromInstanceToLocal | ToInstanceFromLocal
  deriving stock Show

-- | Whether or not to recursively copy files from directories in the
-- @copy-file@ command.
data Recursive = Recursive | NoRecursive
  deriving stock Show

parseCliOpts :: IO CliCmd
parseCliOpts = execParser cliCmdParserInfo

cliCmdParserInfo :: ParserInfo CliCmd
cliCmdParserInfo = info (cliCmdParser <**> helper)
  ( fullDesc <>
    -- progDesc "cloudy" <>
    header "cloudy - create, setup, and manage compute instances in various cloud environments"
  )

cliCmdParser :: Parser CliCmd
cliCmdParser = hsubparser subParsers <|> list
  where
    subParsers =
      awsCommand <>
      scalewayCommand <>
      listCommand <>
      sshCommand <>
      copyFileCommand <>
      destroyCommand

    awsCommand =
      command
        "aws"
        ( info
            (fmap Aws awsCliOptsParser)
            (progDesc "Run AWS-specific commands")
        )

    scalewayCommand =
      command
        "scaleway"
        ( info
            (fmap Scaleway scalewayCliOptsParser)
            (progDesc "Run Scaleway-specific commands")
        )

    listCommand =
      command
        "list"
        ( info
            list
            (progDesc "List currently running compute instances")
        )

    sshCommand =
      command
        "ssh"
        ( info
            (fmap Ssh sshCliOptsParser)
            ( progDesc "SSH to currently running compute instances" <>
              noIntersperse <>
              (footerDoc . Just $
                -- TODO: do this better
                vsep
                  [ "This command internally executes SSH like the following:"
                  , ""
                  , "    $ ssh root@12.34.9.9"
                  , ""
                  , "Any additional arguments specified to this function will be passed to SSH as-is. \
                    \For instance, if you run the following command:"
                  , ""
                  , "    $ cloudy ssh ls /"
                  , ""
                  , "then internally it will execute SSH like the following:"
                  , ""
                  , "    $ ssh root@12.34.9.9 ls /"
                  , ""
                  , "Note that if you want to pass an option to SSH that matches \
                    \an option understood by Cloudy, use \"--\" to separate arguments. \
                    \For instance, if you run the following command:"
                  , ""
                  , "    $ cloudy ssh -i pumpkin-dog -- -i ~/.ssh/my_id_rsa"
                  , ""
                  , "Cloudy will internally execute the following SSH command against the \
                    \instance named \"pumpkin-dog\":"
                  , ""
                  , "    $ ssh root@12.34.9.9 -i ~/.ssh/my_id_rsa"
                  , ""
                  , "SSH also understands the \"--\" argument, so you may need to \
                    \combine these depending on what you're trying to do:"
                  , ""
                  , "    $ cloudy ssh -i pumpkin-dog -- -i ~/.ssh/my_id_rsa -- ls -i /"
                  ]
              )
            )
        )

    copyFileCommand =
      command
        "copy-file"
        ( info
            (fmap CopyFile copyFileCliOptsParser)
            ( progDesc "Copy files to/from currently running compute instances" <>
              forwardOptions <>
              (footerDoc . Just $
                -- TODO: do this better
                vsep
                  [ "Here's an example of using this command to copy files from \
                    \the cloud instance to your local machine:"
                  , ""
                  , "    $ cloudy copy-file -i pumpkin-dog --from-instance my-file-remote1 my-file-remote2 ./my-dir-local/"
                  , ""
                  , "This internally uses SCP to copy files, running a command \
                    \like the following:"
                  , ""
                  , "    $ scp root@12.34.9.9:my-file-remote1 root@12.34.9.9:my-file-remote2 ./my-dir-local/"
                  , ""
                  , "Cloudy will prepend the correct username and IP address to \
                    \all the remote files.  Note that this uses SCP's normal \
                    \rules for paths, so relative paths will be relative to \
                    \the user's HOME directory.  For instance, in the above \
                    \command, \"my-file-remote1\" and \"my-file-remote2\" are \
                    \expected to live in the root user's HOME directory (/root)."
                  , ""
                  , "Here's an example of using this command to copy files from \
                    \your local machine to the cloud instance:"
                  , ""
                  , "    $ cloudy copy-file -i pumpkin-dog --to-instance --recursive my-file-local my-dir-local/ my-dir-remote/"
                  , ""
                  , "This internally runs a command like the following:"
                  , ""
                  , "    $ scp -r my-file-local my-dir-local/ root@12.34.9.9:my-dir-remote/"
                  ]
              )
            )
        )

    destroyCommand =
      command
        "destroy"
        ( info
            (fmap Destroy destroyCliOptsParser)
            ( progDesc "Destroy currently running compute instance" <>
              footer
                "If neither a CLOUDY_INSTANCE_ID nor a CLOUDY_INSTANCE_NAME is \
                \specified, AND there is only a single active Cloudy Instance, \
                \it will be used.  Otherwise, you must specify either \
                \CLOUDY_INSTANCE_ID  or CLOUDY_INSTANCE_NAME, but not both. \
                \Use `cloudy list` to get a list of all active instances ids \
                \and names."
            )
        )

    list = fmap List listCliOptsParser

listCliOptsParser :: Parser ListCliOpts
listCliOptsParser = pure ListCliOpts

sshCliOptsParser :: Parser SshCliOpts
sshCliOptsParser =
  SshCliOpts
    <$> cloudyInstanceIdParser
    <*> cloudyInstanceNameParser
    <*> passthruArgs

copyFileCliOptsParser :: Parser CopyFileCliOpts
copyFileCliOptsParser =
  CopyFileCliOpts
    <$> cloudyInstanceIdParser
    <*> cloudyInstanceNameParser
    <*> directionParser
    <*> recursiveParser
    <*> copyFilesParser

destroyCliOptsParser :: Parser DestroyCliOpts
destroyCliOptsParser =
  DestroyCliOpts
    <$> cloudyInstanceIdParser
    <*> cloudyInstanceNameParser

cloudyInstanceIdParser :: Parser (Maybe CloudyInstanceId)
cloudyInstanceIdParser = fmap (Just . CloudyInstanceId) innerParser <|> pure Nothing
  where
    innerParser =
      option
        auto
        ( long "id" <>
          short 'i' <>
          metavar "CLOUDY_INSTANCE_ID" <>
          help "Cloudy instance ID to operate on."
        )

cloudyInstanceNameParser :: Parser (Maybe Text)
cloudyInstanceNameParser = fmap Just innerParser <|> pure Nothing
  where
    innerParser =
      strOption
        ( long "name" <>
          short 'n' <>
          metavar "CLOUDY_INSTANCE_NAME" <>
          help "Cloudy instance name to operate on."
        )

-- | Parser for arguments that are not really parsed, just passed through.
--
-- Used to pass through arguments to SSH.
passthruArgs :: Parser [Text]
passthruArgs =
  many $
    strArgument
      ( metavar "SSH_ARG..." <>
        help "Arguments to passthru to SSH"
      )

-- | Parser for file names for the copy-files command.
copyFilesParser :: Parser [Text]
copyFilesParser =
  many $
    strArgument
      ( metavar "FILE..." <>
        help "File names to copy to/from"
      )

directionParser :: Parser CopyFileDirection
directionParser =
  let fromInstanceFlag =
        flag'
          FromInstanceToLocal
          ( long "from-instance" <>
            short 'f' <>
            help "Copy files FROM CLOUD INSTANCE to your local machine"
          )
      toInstanceFlag =
        flag'
          FromInstanceToLocal
          ( long "to-instance" <>
            short 't' <>
            help "Copy files from your local machine TO CLOUD INSTANCE"
          )
  in fromInstanceFlag <|> toInstanceFlag

recursiveParser :: Parser Recursive
recursiveParser =
  flag
    NoRecursive
    Recursive
    ( long "recursive" <>
      short 'r' <>
      help "Recursively copy entire directories"
    )
