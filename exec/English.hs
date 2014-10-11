import           Control.Applicative ((<$>))
import           Data.Text           (lines)
import           Data.Text           (unwords)
import           Data.Text.IO        (readFile)
import           Data.Text.IO        (interact)
import           Language.Lexy
import           Prelude             hiding (concat, interact, lines, readFile,
                                      unwords)
import           System.Environment  (getEnv)

main = do
  ws <- lines <$> readFile "/usr/share/dict/words"
  l <- getEnv "LEXER"
  let lexer =case l of
        "trie" -> fetchWordsTrie ws
        "ctk"  -> fetchWords ws
        x -> error ("don't recognise " ++ x)
  interact (unwords . lexer)
