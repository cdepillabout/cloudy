
module Cloudy.Table where

import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty, cons, transpose)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Exception (assert)

data Align = LeftJustified | RightJustified

data Table = Table
  { tableHeaders :: NonEmpty (Align, Text)
  , tableBodyRows :: NonEmpty (NonEmpty Text)
  }

printTable :: Table -> IO ()
printTable Table{tableHeaders, tableBodyRows} = do
  assertRowsSameLengths tableHeaders tableBodyRows
  let rawTable = renderTable tableHeaders tableBodyRows
  Text.putStrLn rawTable

assertRowsSameLengths :: NonEmpty a -> NonEmpty (NonEmpty b) -> IO ()
assertRowsSameLengths headers bodyRows =
  when (any (\row -> length headers /= length row) bodyRows) $
    error "Body row does not contain same number of elements as header row"

renderTable :: NonEmpty (Align, Text) -> NonEmpty (NonEmpty Text) -> Text
renderTable headers body =
  let headerTexts = fmap snd headers
      maxWidths = getMaxWidths $ cons headerTexts body
      maxAlignWidths = NonEmpty.zip (fmap fst headers) maxWidths
      rawHeaders = renderHeaders maxAlignWidths headerTexts
      rawBody = renderBody maxAlignWidths body
      fatDiv = renderDiv True maxWidths "="
      skinnyDiv = renderDiv False maxWidths "-"
  in
  Text.intercalate
    "\n"
    [ skinnyDiv
    , rawHeaders
    , fatDiv
    , rawBody
    , skinnyDiv
    ]

renderDiv :: Bool -> NonEmpty Int -> Text -> Text
renderDiv shouldUseHorizontalDivs maxWidths c =
  let divider = if shouldUseHorizontalDivs then c <> "|" <> c else c <> c <> c
      rowMiddle = Text.intercalate divider . NonEmpty.toList $ fmap (\width -> Text.replicate width c) maxWidths
  in "|" <> c <> rowMiddle <> c <> "|"

renderHeaders :: NonEmpty (Align, Int) -> NonEmpty Text -> Text
renderHeaders maxAlignWidths headers =
  renderRow (zipOneMore maxAlignWidths headers)

renderBody :: NonEmpty (Align, Int) -> NonEmpty (NonEmpty Text) -> Text
renderBody maxAlignWidths body =
  Text.intercalate "\n" $ NonEmpty.toList $ fmap (renderRow . zipOneMore maxAlignWidths) body

zipOneMore :: NonEmpty (a, b) -> NonEmpty c -> NonEmpty (a, b, c)
zipOneMore = NonEmpty.zipWith (\(a, b) c -> (a, b, c))

renderRow :: NonEmpty (Align, Int, Text) -> Text
renderRow columnInfo =
  let rowMiddle = Text.intercalate " | " . NonEmpty.toList $ fmap renderColumn columnInfo
  in "| " <> rowMiddle <> " |"

renderColumn :: (Align, Int, Text) -> Text
renderColumn (align, maxWidth, t) =
  let textWidth = Text.length t
      remainingLength = maxWidth - textWidth
      paddedText =
        case align of
          LeftJustified -> t <> Text.replicate remainingLength " "
          RightJustified -> Text.replicate remainingLength " " <> t
  in assert (remainingLength >= 0) paddedText

getMaxWidths :: NonEmpty (NonEmpty Text) -> NonEmpty Int
getMaxWidths = fmap (maximum . fmap Text.length) . transpose
