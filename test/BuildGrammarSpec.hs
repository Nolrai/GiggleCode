module BuildGrammarSpec
     (spec)
     where
import BuildGrammar
import TestUtils

spec = areInverses ("build", buildGrammar) ("inflate", inflateGrammer) 
