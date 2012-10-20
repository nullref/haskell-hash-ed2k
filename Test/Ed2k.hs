module Main where

import Crypto.Classes
import Crypto.Hash.Ed2k
import Crypto.Types (BitLength)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Int (Int64)
import Data.Tagged
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Text.Printf (printf)

ed2kBlockLength :: Int64
ed2kBlockLength = let bits = untag (blockLength :: Tagged Ed2k BitLength) in fromIntegral $ bits `div` 8

toHex :: Ed2k -> String
toHex d = concatMap (printf "%02x") (B.unpack $ encode $ d)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [testGroup "Hashing tests" [

          testCase "Block length" $
            9728000 @=? ed2kBlockLength,

          testCase "No data (lazy)" $
            "31d6cfe0d16ae931b73c59d7e0c089c0" @=? (toHex $ hash L.empty),
          testCase "No data (strict)" $
            "31d6cfe0d16ae931b73c59d7e0c089c0" @=? (toHex $ hash' B.empty),

          testCase "Short text (lazy)" $
            "1bee69a46ba811185c194762abaeae90" @=? (toHex $ hash (L.pack "The quick brown fox jumps over the lazy dog")),
          testCase "Short text (strict)" $
            "1bee69a46ba811185c194762abaeae90" @=? (toHex $ hash' (B.pack "The quick brown fox jumps over the lazy dog")),

          testCase "Single block, zeroes (lazy)" $
            "d7def262a127cd79096a108e7a9fc138" @=? (toHex $ hash (L.replicate ed2kBlockLength '\0')),
          testCase "Single block, zeroes (strict)" $
            "d7def262a127cd79096a108e7a9fc138" @=? (toHex $ hash' (B.replicate (fromIntegral ed2kBlockLength) '\0')),

          testCase "Two blocks, zeroes (lazy)" $
            "194ee9e4fa79b2ee9f8829284c466051" @=? (toHex $ hash (L.replicate (2 * ed2kBlockLength) '\0')),
          testCase "Two blocks, zeroes (strict)" $
            "194ee9e4fa79b2ee9f8829284c466051" @=? (toHex $ hash' (B.replicate (2 * (fromIntegral ed2kBlockLength)) '\0'))]]
