module Data.SafeCopy.Instances where

import Data.SafeCopy.SafeCopy

import Data.Word
import Data.Int
import Data.Maybe
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B

import Control.Monad



instance SafeCopy a => SafeCopy [a] where
    kind = primitive
    getCopy = contain $
              do n <- get
                 getSafeGet >>= replicateM n
    putCopy lst
        = contain $
          do put (length lst)
             getSafePut >>= forM_ lst

instance SafeCopy a => SafeCopy (Maybe a) where
    kind = primitive
    getCopy = contain $ do n <- get
                           if n then liftM Just safeGet
                                else return Nothing
    putCopy (Just a) = contain $ put True >> safePut a
    putCopy Nothing = contain $ put False

instance (SafeCopy a, Ord a) => SafeCopy (Set.Set a) where
    getCopy = contain $ fmap Set.fromList safeGet
    putCopy = contain . safePut . Set.toList

instance (SafeCopy a,SafeCopy b, Ord a) => SafeCopy (Map.Map a b) where
    getCopy = contain $ fmap Map.fromList safeGet
    putCopy = contain . safePut . Map.toList

instance (SafeCopy a) => SafeCopy (IntMap.IntMap a) where
    getCopy = contain $ fmap IntMap.fromList safeGet
    putCopy = contain . safePut . IntMap.toList


instance (SafeCopy a, SafeCopy b) => SafeCopy (a,b) where
    kind = primitive
    getCopy = contain $ liftM2 (,) safeGet safeGet
    putCopy (a,b) = contain $ safePut a >> safePut b
instance (SafeCopy a, SafeCopy b, SafeCopy c) => SafeCopy (a,b,c) where
    kind = primitive
    getCopy = contain $ liftM3 (,,) safeGet safeGet safeGet
    putCopy (a,b,c) = contain $ safePut a >> safePut b >> safePut c
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d) => SafeCopy (a,b,c,d) where
    kind = primitive
    getCopy = contain $ liftM4 (,,,) safeGet safeGet safeGet safeGet
    putCopy (a,b,c,d) = contain $ safePut a >> safePut b >> safePut c >> safePut d

instance SafeCopy Int where
    kind = Primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Integer where
    kind = Primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Float where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Double where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy L.ByteString where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy B.ByteString where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Char where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Word8 where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Word16 where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Word32 where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Word64 where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Ordering where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Int8 where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Int16 where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Int32 where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Int64 where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy () where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance SafeCopy Bool where
    kind = primitive; getCopy = contain $ get; putCopy = contain . put
instance (SafeCopy a, SafeCopy b) => SafeCopy (Either a b) where
    kind = primitive
    getCopy = contain $ do n <- get
                           if n then liftM Right safeGet
                                else liftM Left safeGet
    putCopy (Right a) = contain $ put True >> safePut a
    putCopy (Left a) = contain $ put False >> safePut a


