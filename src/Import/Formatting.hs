-- | Text formatting operations.

module Import.Formatting
  (limitLen
  ,limitByWords
  ,serialComma)
  where

import           ClassyPrelude
import           Data.List (init, last)
import qualified Data.Text as T

-- | Limit the length of text and ellipsize if the text is
-- truncated. Can be used in composition with "limitByWords" as
--
-- >>> limitLen a . limitByWords b
limitLen :: Int -> Text -> Text
limitLen n xs | T.length xs > n = dropWhile (=='…') (T.take (max 1 (n-1)) xs) <> "…"
              | otherwise       = xs

-- | Limit the length of text and ellipsize if the text is truncated.
limitByWords :: Int -> Text -> Text
limitByWords n xs | length parts > n = T.intercalate " " (take n parts) <> "…"
                  | otherwise        = xs

  where parts = T.words xs

-- Insert commas and \"and\" into a list of 'Text', as appropriate.
serialComma :: [Text] -> Text
serialComma [x] = x
serialComma [x, y] = x ++ " and " ++ y
serialComma xs = concat (map (++ ", ") (Data.List.init xs)) ++ "and " ++ Data.List.last xs
