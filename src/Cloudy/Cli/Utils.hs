
module Cloudy.Cli.Utils where

import Options.Applicative (Alternative((<|>)), Parser, help, Mod)

-- | This lifts a option 'Parser' to a 'Parser' of 'Maybe', allowing you to
-- specify a default value.
--
-- Given a call like:
--
-- @
--   'maybeOpt' \"Which foobar to use\" \"bazqux\" 'strOption' ('short' \'f\' <> 'metavar' \"FOOBAR\") :: 'Parser' (Maybe 'Text')
-- @
--
-- this returns 'Nothing' if the user doesn't specify the @-f@ option, and
-- 'Just' if the user does.  It also shows that the default values is @\"bazqux\"@.
--
-- Using 'maybeOpt' is different than just using the 'value' 'Mod' in order to
-- set a default value, since 'maybeOpt' returns 'Nothing' if the option was
-- not given on the command line (but it still shows the default value in the
-- @--help@ output.
maybeOpt :: Show a => String -> a -> (Mod f a -> Parser a) -> Mod f a -> Parser (Maybe a)
maybeOpt helpStr defaultVal p mods =
  fmap Just (p $ mods <> help helpWithDefaultStr) <|> pure Nothing
  where
    helpWithDefaultStr = helpStr <> " (default: " <> show defaultVal <> ")"
