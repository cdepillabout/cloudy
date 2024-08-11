
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
import Options.Applicative
  ( Alternative((<|>)), Parser, (<**>), command, fullDesc, header, info
  , progDesc, execParser, helper, hsubparser, ParserInfo )

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
            (progDesc "Destroy currently running compute instance")
        )

    list = fmap List listCliOptsParser

listCliOptsParser :: Parser ListCliOpts
listCliOptsParser = pure ListCliOpts

sshCliOptsParser :: Parser SshCliOpts
sshCliOptsParser = pure SshCliOpts

destroyCliOptsParser :: Parser DestroyCliOpts
destroyCliOptsParser = pure DestroyCliOpts
