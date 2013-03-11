{-# LANGUAGE MultiParamTypeClasses #-}
module Crypto.Hash.Ed2k (
  Ctx,
  Ed2k
  ) where

import Crypto.Classes
import qualified Crypto.Hash.MD4 as MD4
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (foldl')
import Data.Tagged (Tagged(..))
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (getByteString)
import Data.Serialize.Put (putByteString)

data Ed2k = Digest ByteString
  deriving (Eq, Ord)

ed2kBlock :: Int
ed2kBlock = 9728000

instance Serialize Ed2k where
  get = fmap Digest $ getByteString 16
  put (Digest d) = putByteString d

data Ctx
  = InitCtx
  | SingleCtx !ByteString
  | MultiCtx !MD4.Ctx

appendHash :: Ctx -> ByteString -> Ctx
appendHash InitCtx        d = SingleCtx d
appendHash (SingleCtx b)  d = MultiCtx $ MD4.update MD4.init $ (b `B.append` d)
appendHash (MultiCtx ctx) d = MultiCtx $ MD4.update ctx d

splitBS :: ByteString -> Int -> [ByteString]
splitBS b n
  | B.null b       = []
  | B.length b < n = undefined
  | otherwise      = let (h, t) = B.splitAt n b in h : splitBS t n

instance Hash Ctx Ed2k where
  outputLength = Tagged $ 16 * 8

  blockLength = Tagged $ ed2kBlock * 8

  initialCtx = InitCtx

  updateCtx ctx b
    | B.null b                       = ctx
    | B.length b == ed2kBlock        = appendHash ctx $ MD4.hash b
    -- The size of the input ByteString is required by the API
    -- to be a multiple of the blockLength.
    | B.length b `rem` ed2kBlock > 0 = undefined
    -- B.length b > ed2kBlock
    | otherwise                      = foldl' (\ ctx' b' -> appendHash ctx' $ MD4.hash b') ctx (splitBS b ed2kBlock)

  finalize InitCtx           b            = Digest $ MD4.finalize $ MD4.update MD4.init b
  finalize (SingleCtx d)     b | B.null b = Digest $ d
  finalize ctx@(SingleCtx _) b            = finalize (appendHash ctx b) B.empty
  finalize (MultiCtx ctx)    b            = Digest $ MD4.finalize $ MD4.update ctx (MD4.hash b)
