{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns #-}

module Main where

import Lib
import Data.Text.Lazy as T
import Data.Text.Lazy.IO as T
import Data.ByteString.Lazy as B
import Prelude (IO, FilePath, Either(..), ($), (.))
import Control.Applicative (pure, (<$>))
import System.Environment (getArgs)
import Control.Monad.Exception
import System.Exit (die)
import Control.DeepSeq(force, NFData)

main :: IO ()
main =
  do
  [flag, src, dst] <- getArgs
  f <- case flag of
    "-c" -> body compress
    "-d" -> body decompress
  f src dst

body :: (File a, File b, NFData b) => (a -> EMG b) -> FilePath -> FilePath -> IO ()
body f src dst =
  do
  input <- readFileC src
  T.putStrLn . T.pack $ src ++ " read"
  output <- dieOnException (f input)
  T.putStrLn . T.pack $ src ++ " (de)compressed."
  writeFileC dst output
  T.putStrLn . T.pack $ "Result writen to " ++ dst

class File t where
  readFileC :: FilePath -> IO t
  writeFileC :: FilePath -> t -> IO ()

instance File T.Text where
  readFileC = T.readFile
  writeFileC = T.writeFile

instance File ByteString where
  readFileC = B.readFile
  writeFileC = B.writeFile

dieOnException :: NFData a => EM AnyException a -> IO a
dieOnException m =
  case tryEMWithLoc (force <$> m) of
    Right a -> pure a
    Left (loc, e) -> die $ showExceptionWithTrace loc e
