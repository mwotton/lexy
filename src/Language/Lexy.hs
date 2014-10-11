{-# LANGUAGE TupleSections #-}
module Language.Lexy where

import           Control.Arrow                ((&&&))
import           Data.Char                    (isPunctuation)
import           Data.List                    (group, sort, sortBy)
import           Data.List                    (foldl1')
import           Data.Ord                     (comparing)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Text.CTK.Lexers
import           Text.Pandoc                  (def)
import           Text.Pandoc.Readers.HTML
import           Text.Pandoc.Writers.Markdown
--import           Text.Regex.Posix
--import           Text.Regex.TDFA.Text
import qualified Data.ByteString              as BS
import           Data.Text.Encoding           (encodeUtf8)
import           Data.Text.Encoding           (decodeUtf8)
import           Data.Trie                    (fromList, match)

sep x = isPunctuation x || x == '\n'

grabText :: String -> [Text]
grabText = Text.split sep . Text.pack . writeMarkdown def . readHtml def


frequency :: Ord a => (Text -> [a]) -> String -> [(a, Int)]
frequency tokeniser  = sortBy (comparing snd) . map (head &&& length) . group . sort . concatMap tokeniser . grabText

notWordChar = alt " -,!@#$%^&*(){}?<>."

fetchWords :: [Text] -> Text -> [Text]
fetchWords dict = let goodstuff = lexaction (foldl1' (>|<) $ map (string . Text.unpack) dict) (\s p -> Just s)
                      rubbish = lexaction (notWordChar +> many notWordChar) (\_ _ -> Nothing)
                  in \input -> let
                    (result,_,_) = execLexer (goodstuff >||< rubbish) (Text.unpack input, ("",0,0), ())
                    in map (Text.pack) result

many re = \l' -> let self = re self >||< l' in self

fetchWordsTrie :: [Text] -> Text -> [Text]
fetchWordsTrie dict = let trie = fromList $ map ((,()) . encodeUtf8) dict
                          go line = case (BS.null line, match trie line) of
                            (True,_) -> []
                            (_,Nothing) -> go (BS.drop 1 line)
                            (_,Just (prefix,_,remainder)) -> (decodeUtf8 prefix:go remainder)
                      in go . encodeUtf8
