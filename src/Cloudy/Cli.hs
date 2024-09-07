
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
  , progDesc, execParser, helper, hsubparser, ParserInfo, strOption, long, short, metavar, help, option, auto )
import Options.Applicative.Builder (footer)

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
            (progDesc "SSH to currently running compute instances")
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
