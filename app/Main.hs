module Main where

import Lib
import Data.Text.Lazy as T
import Data.Text.Lazy.IO as T
import Data.ByteString.Lazy as B
import Prelude (IO, FilePath, error)
import System.Environment (getArgs)

main :: IO ()
main =
    do
    [flag, src, dst] <- getArgs
    let f = case flag of
              "-c" -> body compress
              "-d" -> body decompress
              _ -> error "requires -c or -d as first imput"
    f src dst

body :: (File a, File b) => (a -> b) -> FilePath -> FilePath -> IO () 
body f src dst = 
  do
  input <- readFileC src
  writeFileC dst (f input)

class File c where
  readFileC :: FilePath -> IO c
  writeFileC :: FilePath -> c -> IO () 

instance File T.Text where
  readFileC = T.readFile
  writeFileC = T.writeFile

instance File ByteString where
  readFileC = B.readFile
  writeFileC = B.writeFile
