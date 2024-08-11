
module Cloudy.Cli
  ( parseCliOpts
  , CliCmd(..)
  , CreateCliOpts(..)
  , ScalewayCliOpts(..)
  , AwsCliOpts(..)
  , ListCliOpts(..)
  , SshCliOpts(..)
  , DestroyCliOpts(..)
  )
  where

import Cloudy.Cli.CreateAws (AwsCliOpts(..), awsCliOptsParser)
import Cloudy.Cli.CreateScaleway (ScalewayCliOpts(..), scalewayCliOptsParser)
import Options.Applicative

data CliCmd
  = Create CreateCliOpts
  | List ListCliOpts
  | Ssh SshCliOpts
  | Destroy DestroyCliOpts
  deriving stock Show

data CreateCliOpts
  = CreateCliOptsScaleway ScalewayCliOpts
  | CreateCliOptsAws AwsCliOpts
  deriving stock Show

data ListCliOpts = ListCliOpts
  deriving stock Show

data SshCliOpts = SshCliOpts
  deriving stock Show

data DestroyCliOpts = DestroyCliOpts
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
    subParsers = createCommand <> listCommand <> sshCommand <> destroyCommand

    createCommand =
      command
        "create"
        ( info
            (fmap Create createCliOptsParser)
            (progDesc "Create a new compute instance")
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
            (progDesc "SSH to currently running compute instances")
        )

    destroyCommand =
      command
        "destroy"
        ( info
            (fmap Destroy destroyCliOptsParser)
            (progDesc "Destroy currently running compute instance")
        )

    list = fmap List listCliOptsParser

createCliOptsParser :: Parser CreateCliOpts
createCliOptsParser = hsubparser subParsers
  where
    subParsers = scalewayCommand <> awsCommand

    scalewayCommand =
      command
        "scaleway"
        ( info
            (fmap CreateCliOptsScaleway scalewayCliOptsParser)
            (progDesc "Create a new compute instance in Scaleway")
        )

    awsCommand =
      command
        "aws"
        ( info
            (fmap CreateCliOptsAws awsCliOptsParser)
            (progDesc "Create a new compute instance in AWS")
        )


listCliOptsParser :: Parser ListCliOpts
listCliOptsParser = pure ListCliOpts

sshCliOptsParser :: Parser SshCliOpts
sshCliOptsParser = pure SshCliOpts

destroyCliOptsParser :: Parser DestroyCliOpts
destroyCliOptsParser = pure DestroyCliOpts
