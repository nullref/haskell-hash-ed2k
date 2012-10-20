module Main where

import Crypto.Classes
import Crypto.Hash.Ed2k
import qualified Data.ByteString as B
import Data.Word (Word8)
import System.Exit

testHash :: [Word8]
testHash = [83,211,145,194,240,9,199,230,55,34,205,252,35,114,246,228]

main :: IO ()
main = do
  testData <- B.readFile "testData.bin"
  let testHash' = B.unpack $ encode ((hash' testData) :: Ed2k)
  if testHash' == testHash
    then exitSuccess
    else do
      print testHash'
      exitFailure
