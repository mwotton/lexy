module Main where
import           Control.Applicative ((<$>))
import           Criterion.Main
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import           Language.Lexy

main :: IO ()
main = do
  dict <- Text.lines <$> Text.readFile "/usr/share/dict/words"
  testInput <- Text.readFile "./all.html"
  defaultMain [
    bgroup "lexers"
      [ bench "ctk"  $ nf (fetchWords dict) testInput
      , bench "trie"  $ nf (fetchWordsTrie dict) testInput
      ]
    ]
