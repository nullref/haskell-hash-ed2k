module Main where

import Crypto.Classes
import Crypto.Hash.MD4 (MD4)
import qualified Data.ByteString as B
import Data.Word (Word8)
import System.Exit

testHash :: [Word8]
testHash = [191,149,85,7,66,43,39,195,223,16,218,220,213,120,39,11]

main :: IO ()
main = do
  testData <- B.readFile "testData.bin"
  let testHash' = B.unpack $ encode ((hash' testData) :: MD4)
  if testHash' == testHash
    then exitSuccess
    else do
      print testHash'
      exitFailure
