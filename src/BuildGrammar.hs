module BuildGrammar
    ( buildGrammar
    , inflateGrammar
    ) where
import qualified Data.Text as T
import Grammar

buildGrammar :: T.Text -> Grammar Char
buildGrammar = undefined

inflateGrammar :: Grammar Char -> T.Text
inflateGrammar = undefined
