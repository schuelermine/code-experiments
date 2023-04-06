module DisplayPairs where

import Data.Char (isSpace)
import Data.List (nub)
import Data.Text (Text, unpack, pack, toUpper, toLower)

displayPair :: Char -> Text
displayPair char =
  let
    text = pack [char]
    upper = toUpper text
    lower = toLower text
  in mconcat $ nub [upper, text, lower]
  
genDisplayPairs :: String -> String
genDisplayPairs = unwords . nub . (unpack . displayPair <$>) . filter (not . isSpace)
