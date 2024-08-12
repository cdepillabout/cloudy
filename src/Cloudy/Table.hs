
module Cloudy.Table where

import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty((:|)), cons, transpose)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data Table = Table
  { headers :: NonEmpty Text
  , body :: NonEmpty (NonEmpty Text)
  }

printTable :: Table -> IO ()
printTable Table{headers, body} = do
  assertRowsSameLengths (cons headers body)
  let rawTable = renderTable headers body
  Text.putStrLn rawTable

renderTable :: NonEmpty Text -> NonEmpty (NonEmpty Text) -> Text
renderTable heads bod =
  let maxWidths = getMaxWidths (cons heads bod)
      rawHeaders = renderHeaders maxWidths heads
      rawBody = renderBody bod
  in rawHeaders <> rawBody

getMaxWidths :: NonEmpty (NonEmpty Text) -> NonEmpty Int
getMaxWidths = fmap (maximum . fmap Text.length) . transpose

assertRowsSameLengths :: NonEmpty (NonEmpty a) -> IO ()
assertRowsSameLengths (h :| ts) =
  when (any (\row -> length h /= length row) ts) $
    error "Body row does not have same number of elements as header row"
