
module Cloudy.Cli
  ( parseCliOpts
  , CliCmd(..)
  , ScalewayCliOpts(..)
  , AwsCliOpts(..)
  , ListCliOpts(..)
  , SshCliOpts(..)
  , DestroyCliOpts(..)
  )
  where

import Cloudy.Cli.Aws (AwsCliOpts(..), awsCliOptsParser)
import Cloudy.Cli.Scaleway (ScalewayCliOpts(..), scalewayCliOptsParser)
import Cloudy.Db (CloudyInstanceId (..))
import Data.Text (Text)
import Options.Applicative
  ( Alternative((<|>)), Parser, (<**>), command, fullDesc, header, info
  , progDesc, execParser, helper, footer, hsubparser, ParserInfo, strOption, long, short, metavar, help, option, auto, noIntersperse, forwardOptions, strArgument )
import Control.Applicative (Alternative(many))

data CliCmd
  = Aws AwsCliOpts
  | List ListCliOpts
  | Scaleway ScalewayCliOpts
  | Ssh SshCliOpts
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

data DestroyCliOpts = DestroyCliOpts
  { id :: Maybe CloudyInstanceId
  , name :: Maybe Text
  }
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
    subParsers = awsCommand <> listCommand <> scalewayCommand <> sshCommand <> destroyCommand

    awsCommand =
      command
        "aws"
        ( info
            (fmap Aws awsCliOptsParser)
            (progDesc "Run AWS-specific commands")
        )

    listCommand =
      command
        "list"
        ( info
            list
            (progDesc "List currently running compute instances")
        )

    scalewayCommand =
      command
        "scaleway"
        ( info
            (fmap Scaleway scalewayCliOptsParser)
            (progDesc "Run Scaleway-specific commands")
        )

    sshCommand =
      command
        "ssh"
        ( info
            (fmap Ssh sshCliOptsParser)
            ( progDesc "SSH to currently running compute instances" <>
              noIntersperse <>
              forwardOptions <>
              footer
                "This command internally execs SSH like the following:\n\n\
                \  $ ssh root@123.234.9.9\n\n\
                \Any additional arguments specified to this function will be passed to SSH as-is. \
                \For instance, if you run the following command:\n\n\
                \  $ cloudy ssh ls /\n\n\
                \then internally it will exec SSH like the following:\n\n\
                \  $ ssh root@123.234.9.9 ls /\n\n\
                \Note that if you want to pass an option to SSH that matches \
                \an option understood by Cloudy, use \"--\" to separate arguments. \
                \For instance, if you run the following command:\n\n\
                \  $ cloudy ssh -i pumpkin-dog -- -i ~/.ssh/my_id_rsa\n\n\
                \Cloudy will internally exec the following SSH command against the \
                \instance called \"pumpkin-dog\":\n\n\
                \  $ ssh root@123.234.9.9 -i ~/.ssh/my_id_rsa\n\n\
                \SSH also understands the \"--\" argument, so you may need to \
                \combine these depending on what you're trying to do:\n\n\
                \  $ cloudy ssh -i pumpkin-dog -- -i ~/.ssh/my_id_rsa -- ls -i /"
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
passthruArgs :: Parser [Text]
passthruArgs =
  many $
    strArgument
      ( metavar "SSH_ARG..." <>
        help "Arguments to passthru to SSH"
      )

