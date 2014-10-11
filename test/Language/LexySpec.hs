{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Language.LexySpec where
import           Control.Applicative ((<$>))
import           Data.FileEmbed
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import qualified Data.Text.IO        as Text
import           Language.Lexy
import           Test.Hspec
import           Test.QuickCheck

ws = $(embedFile "/usr/share/dict/words")

instance Arbitrary Text.Text where
  arbitrary = Text.pack <$> arbitrary

spec = describe "ctk vs trie" $
       it "gets the same" $
         let dict =  Text.lines $ Text.decodeUtf8 ws
             t = fetchWordsTrie dict
             c = fetchWords dict
         in property $ \x -> t x == c x
