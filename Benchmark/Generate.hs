module Main where

import qualified Data.ByteString as B

testData :: B.ByteString
testData = B.concat
  [ B.replicate 10000000 0
  , B.replicate 10000000 1
  , B.replicate 10000000 2
  , B.replicate 10000000 3
  , B.replicate 10000000 4
  , B.replicate 10000000 5
  , B.replicate 10000000 6
  , B.replicate 10000000 7
  , B.replicate 10000000 8
  , B.replicate 10000000 9
  , B.replicate 10000000 10
  , B.replicate 10000000 11
  , B.replicate 10000000 12
  , B.replicate 10000000 13
  , B.replicate 10000000 14
  , B.replicate 10000000 15
  ]

-- Generates the file testData.bin in the current directory.
-- This file is used as input to the benchmarks.
main :: IO ()
main = do
  B.writeFile "testData.bin" testData
