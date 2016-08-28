module BuildGrammarSpec
     (spec)
     where
import BuildGrammar
import TestUtils
import GrammarSpec ()

spec :: Spec
spec = areInverses ("build", buildGrammar) ("inflate", inflateGrammar) 
