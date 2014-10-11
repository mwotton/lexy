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
  print (Text.length testInput)
  defaultMain [
    bgroup "lexers"
      [ bench "ctk"  $ whnf (last . fetchWords dict) testInput
      , bench "trie"  $ whnf (last . fetchWordsTrie dict) testInput
      ]
    ]
