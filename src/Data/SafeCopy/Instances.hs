{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, UndecidableInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.SafeCopy.Instances where

import Data.SafeCopy.SafeCopy

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad
import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as Foldable
import           Data.Fixed (HasResolution, Fixed)
import           Data.Int
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.Ix
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Ratio (Ratio, (%), numerator, denominator)
import qualified Data.Sequence as Sequence
import           Data.Serialize
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (DiffTime, NominalDiffTime, UniversalTime(..), UTCTime(..))
import           Data.Time.Clock.TAI (AbsoluteTime, taiEpoch, addAbsoluteTime, diffAbsoluteTime)
import           Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), TimeZone(..), ZonedTime(..))
import qualified Data.Tree as Tree
#if MIN_VERSION_base(4,7,0)
import           Data.Typeable hiding (Proxy)
#else
import           Data.Typeable
#endif
import           Data.Word
#if MIN_VERSION_base(4,8,0)
import           Numeric.Natural (Natural)
#endif
import           System.Time (ClockTime(..), TimeDiff(..), CalendarTime(..), Month(..))
import qualified System.Time as OT
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

instance SafeCopy a => SafeCopy (Prim a) where
  kind = primitive
  getCopy = contain $
            do e <- unsafeUnPack getCopy
               return $ Prim e
  putCopy (Prim e)
    = contain $ unsafeUnPack (putCopy e)

instance SafeCopy a => SafeCopy [a] where
  getCopy = contain $ do
    n <- get
    g <- getSafeGet
    go g [] n
      where
        go :: Get a -> [a] -> Int -> Get [a]
        go _ as 0 = return (reverse as)
        go g as i = do x <- g
                       x `seq` go g (x:as) (i - 1)
  putCopy lst = contain $ do put (length lst)
                             getSafePut >>= forM_ lst
  errorTypeName = typeName1

instance SafeCopy a => SafeCopy (NonEmpty.NonEmpty a) where
    getCopy = contain $ fmap NonEmpty.fromList safeGet
    putCopy = contain . safePut . NonEmpty.toList
    errorTypeName = typeName1

instance SafeCopy a => SafeCopy (Maybe a) where
    getCopy = contain $ do n <- get
                           if n then liftM Just safeGet
                                else return Nothing
    putCopy (Just a) = contain $ put True >> safePut a
    putCopy Nothing = contain $ put False
    errorTypeName = typeName1

instance (SafeCopy a, Ord a) => SafeCopy (Set.Set a) where
    getCopy = contain $ fmap Set.fromDistinctAscList safeGet
    putCopy = contain . safePut . Set.toAscList
    errorTypeName = typeName1

instance (SafeCopy a, SafeCopy b, Ord a) => SafeCopy (Map.Map a b) where
    getCopy = contain $ fmap Map.fromDistinctAscList safeGet
    putCopy = contain . safePut . Map.toAscList
    errorTypeName = typeName2

instance (SafeCopy a) => SafeCopy (IntMap.IntMap a) where
    getCopy = contain $ fmap IntMap.fromDistinctAscList safeGet
    putCopy = contain . safePut . IntMap.toAscList
    errorTypeName = typeName1

instance SafeCopy IntSet.IntSet where
    getCopy = contain $ fmap IntSet.fromDistinctAscList safeGet
    putCopy = contain . safePut . IntSet.toAscList
    errorTypeName = typeName

instance (SafeCopy a) => SafeCopy (Sequence.Seq a) where
    getCopy = contain $ fmap Sequence.fromList safeGet
    putCopy = contain . safePut . Foldable.toList
    errorTypeName = typeName1

instance (SafeCopy a) => SafeCopy (Tree.Tree a) where
    getCopy = contain $ liftM2 Tree.Node safeGet safeGet
    putCopy (Tree.Node root sub) = contain $ safePut root >> safePut sub
    errorTypeName = typeName1

iarray_getCopy :: (Ix i, SafeCopy e, SafeCopy i, IArray.IArray a e) => Contained (Get (a i e))
iarray_getCopy = contain $ do getIx <- getSafeGet
                              liftM3 mkArray getIx getIx safeGet
    where
      mkArray l h xs = IArray.listArray (l, h) xs
{-# INLINE iarray_getCopy #-}

iarray_putCopy :: (Ix i, SafeCopy e, SafeCopy i, IArray.IArray a e) => a i e -> Contained Put
iarray_putCopy arr = contain $ do putIx <- getSafePut
                                  let (l,h) = IArray.bounds arr
                                  putIx l >> putIx h
                                  safePut (IArray.elems arr)
{-# INLINE iarray_putCopy #-}

instance (Ix i, SafeCopy e, SafeCopy i) => SafeCopy (Array.Array i e) where
    getCopy = iarray_getCopy
    putCopy = iarray_putCopy
    errorTypeName = typeName2

instance (IArray.IArray UArray.UArray e, Ix i, SafeCopy e, SafeCopy i) => SafeCopy (UArray.UArray i e) where
    getCopy = iarray_getCopy
    putCopy = iarray_putCopy
    errorTypeName = typeName2

instance (SafeCopy a, SafeCopy b) => SafeCopy (a,b) where
    getCopy = contain $ liftM2 (,) safeGet safeGet
    putCopy (a,b) = contain $ safePut a >> safePut b
    errorTypeName = typeName2
instance (SafeCopy a, SafeCopy b, SafeCopy c) => SafeCopy (a,b,c) where
    getCopy = contain $ liftM3 (,,) safeGet safeGet safeGet
    putCopy (a,b,c) = contain $ safePut a >> safePut b >> safePut c
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d) => SafeCopy (a,b,c,d) where
    getCopy = contain $ liftM4 (,,,) safeGet safeGet safeGet safeGet
    putCopy (a,b,c,d) = contain $ safePut a >> safePut b >> safePut c >> safePut d
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e) =>
         SafeCopy (a,b,c,d,e) where
    getCopy = contain $ liftM5 (,,,,) safeGet safeGet safeGet safeGet safeGet
    putCopy (a,b,c,d,e) = contain $ safePut a >> safePut b >> safePut c >> safePut d >> safePut e
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e, SafeCopy f) =>
         SafeCopy (a,b,c,d,e,f) where
    getCopy = contain $ (,,,,,) <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet
    putCopy (a,b,c,d,e,f) = contain $ safePut a >> safePut b >> safePut c >> safePut d >>
                                      safePut e >> safePut f
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e, SafeCopy f, SafeCopy g) =>
         SafeCopy (a,b,c,d,e,f,g) where
    getCopy = contain $ (,,,,,,) <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*>
                                     safeGet <*> safeGet <*> safeGet
    putCopy (a,b,c,d,e,f,g) = contain $ safePut a >> safePut b >> safePut c >> safePut d >>
                                        safePut e >> safePut f >> safePut g


instance SafeCopy Int where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Integer where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
#if MIN_VERSION_base(4,8,0)
instance SafeCopy Natural where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
#endif

-- | cereal change the formats for Float/Double in 0.5.*
--
-- https://github.com/GaloisInc/cereal/commit/47d839609413e3e9d1147b99c34ae421ae36bced
-- https://github.com/GaloisInc/cereal/issues/35
newtype CerealFloat040 = CerealFloat040 { unCerealFloat040 :: Float} deriving (Show, Typeable)
instance SafeCopy CerealFloat040 where
    getCopy = contain (CerealFloat040 <$> liftM2 encodeFloat get get)
    putCopy (CerealFloat040 float) = contain (put (decodeFloat float))
    errorTypeName = typeName

instance Migrate Float where
  type MigrateFrom Float = CerealFloat040
  migrate (CerealFloat040 d) = d

instance SafeCopy Float where
  version = Version 1
  kind = extension
  getCopy = contain get
  putCopy = contain . put
  errorTypeName = typeName

-- | cereal change the formats for Float/Double in 0.5.*
--
-- https://github.com/GaloisInc/cereal/commit/47d839609413e3e9d1147b99c34ae421ae36bced
-- https://github.com/GaloisInc/cereal/issues/35
newtype CerealDouble040 = CerealDouble040 { unCerealDouble040 :: Double} deriving (Show, Typeable)
instance SafeCopy CerealDouble040 where
    getCopy = contain (CerealDouble040 <$> liftM2 encodeFloat get get)
    putCopy (CerealDouble040 double) = contain (put (decodeFloat double))
    errorTypeName = typeName

instance Migrate Double where
  type MigrateFrom Double = CerealDouble040
  migrate (CerealDouble040 d) = d

instance SafeCopy Double where
  version = Version 1
  kind = extension
  getCopy = contain get
  putCopy = contain . put
  errorTypeName = typeName


instance SafeCopy L.ByteString where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy B.ByteString where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Char where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Word where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Word8 where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Word16 where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Word32 where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Word64 where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Ordering where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Int8 where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Int16 where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Int32 where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Int64 where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance (Integral a, SafeCopy a) => SafeCopy (Ratio a) where
    getCopy   = contain $ do n <- safeGet
                             d <- safeGet
                             return (n % d)
    putCopy r = contain $ do safePut (numerator   r)
                             safePut (denominator r)
    errorTypeName = typeName1
instance (HasResolution a, Fractional (Fixed a)) => SafeCopy (Fixed a) where
    getCopy   = contain $ fromRational <$> safeGet
    putCopy   = contain . safePut . toRational
    errorTypeName = typeName1

instance SafeCopy () where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Bool where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance (SafeCopy a, SafeCopy b) => SafeCopy (Either a b) where
    getCopy = contain $ do n <- get
                           if n then liftM Right safeGet
                                else liftM Left safeGet
    putCopy (Right a) = contain $ put True >> safePut a
    putCopy (Left a) = contain $ put False >> safePut a

    errorTypeName = typeName2

--  instances for 'text' library

instance SafeCopy T.Text where
    kind = base
    getCopy = contain $ T.decodeUtf8 <$> safeGet
    putCopy = contain . safePut . T.encodeUtf8
    errorTypeName = typeName

instance SafeCopy TL.Text where
    kind = base
    getCopy = contain $ TL.decodeUtf8 <$> safeGet
    putCopy = contain . safePut . TL.encodeUtf8
    errorTypeName = typeName

-- instances for 'time' library

instance SafeCopy Day where
    kind = base
    getCopy = contain $ ModifiedJulianDay <$> safeGet
    putCopy = contain . safePut . toModifiedJulianDay
    errorTypeName = typeName

instance SafeCopy DiffTime where
    kind = base
    getCopy = contain $ fromRational <$> safeGet
    putCopy = contain . safePut . toRational
    errorTypeName = typeName

instance SafeCopy UniversalTime where
    kind = base
    getCopy = contain $ ModJulianDate <$> safeGet
    putCopy = contain . safePut . getModJulianDate
    errorTypeName = typeName

instance SafeCopy UTCTime where
    kind = base
    getCopy   = contain $ do day      <- safeGet
                             diffTime <- safeGet
                             return (UTCTime day diffTime)
    putCopy u = contain $ do safePut (utctDay u)
                             safePut (utctDayTime u)
    errorTypeName = typeName

instance SafeCopy NominalDiffTime where
    kind = base
    getCopy = contain $ fromRational <$> safeGet
    putCopy = contain . safePut . toRational
    errorTypeName = typeName

instance SafeCopy TimeOfDay where
    kind = base
    getCopy   = contain $ do hour <- safeGet
                             mins <- safeGet
                             sec  <- safeGet
                             return (TimeOfDay hour mins sec)
    putCopy t = contain $ do safePut (todHour t)
                             safePut (todMin t)
                             safePut (todSec t)
    errorTypeName = typeName

instance SafeCopy TimeZone where
    kind = base
    getCopy   = contain $ do mins       <- safeGet
                             summerOnly <- safeGet
                             zoneName   <- safeGet
                             return (TimeZone mins summerOnly zoneName)
    putCopy t = contain $ do safePut (timeZoneMinutes t)
                             safePut (timeZoneSummerOnly t)
                             safePut (timeZoneName t)
    errorTypeName = typeName

instance SafeCopy LocalTime where
    kind = base
    getCopy   = contain $ do day <- safeGet
                             tod <- safeGet
                             return (LocalTime day tod)
    putCopy t = contain $ do safePut (localDay t)
                             safePut (localTimeOfDay t)
    errorTypeName = typeName

instance SafeCopy ZonedTime where
    kind = base
    getCopy   = contain $ do localTime <- safeGet
                             timeZone  <- safeGet
                             return (ZonedTime localTime timeZone)
    putCopy t = contain $ do safePut (zonedTimeToLocalTime t)
                             safePut (zonedTimeZone t)
    errorTypeName = typeName

instance SafeCopy AbsoluteTime where
  getCopy = contain $ liftM toAbsoluteTime safeGet
    where
      toAbsoluteTime :: DiffTime -> AbsoluteTime
      toAbsoluteTime dt = addAbsoluteTime dt taiEpoch
  putCopy = contain . safePut . fromAbsoluteTime
    where
      fromAbsoluteTime :: AbsoluteTime -> DiffTime
      fromAbsoluteTime at = diffAbsoluteTime at taiEpoch
  errorTypeName = typeName

-- instances for old-time

instance SafeCopy ClockTime where
    kind = base
    getCopy = contain $ do secs <- safeGet
                           pico <- safeGet
                           return (TOD secs pico)
    putCopy (TOD secs pico) =
              contain $ do safePut secs
                           safePut pico

instance SafeCopy TimeDiff where
    kind = base
    getCopy   = contain $ do year    <- get
                             month   <- get
                             day     <- get
                             hour    <- get
                             mins    <- get
                             sec     <- get
                             pico    <- get
                             return (TimeDiff year month day hour mins sec pico)
    putCopy t = contain $ do put (tdYear t)
                             put (tdMonth t)
                             put (tdDay t)
                             put (tdHour t)
                             put (tdMin t)
                             put (tdSec t)
                             put (tdPicosec t)

instance SafeCopy OT.Day where
    kind = base ; getCopy = contain $ toEnum <$> get ; putCopy = contain . put . fromEnum

instance SafeCopy Month where
    kind = base ; getCopy = contain $ toEnum <$> get ; putCopy = contain . put . fromEnum


instance SafeCopy CalendarTime where
    kind = base
    getCopy   = contain $ do year   <- get
                             month  <- safeGet
                             day    <- get
                             hour   <- get
                             mins   <- get
                             sec    <- get
                             pico   <- get
                             wday   <- safeGet
                             yday   <- get
                             tzname <- safeGet
                             tz     <- get
                             dst    <- get
                             return (CalendarTime year month day hour mins sec pico wday yday tzname tz dst)
    putCopy t = contain $ do put     (ctYear t)
                             safePut (ctMonth t)
                             put     (ctDay t)
                             put     (ctHour t)
                             put     (ctMin t)
                             put     (ctSec t)
                             put     (ctPicosec t)
                             safePut (ctWDay t)
                             put     (ctYDay t)
                             safePut (ctTZName t)
                             put     (ctTZ t)
                             put     (ctIsDST t)

typeName :: Typeable a => Proxy a -> String
typeName proxy = show (typeOf (undefined `asProxyType` proxy))

#if MIN_VERSION_base(4,10,0)
typeName1 :: (Typeable c) => Proxy (c a) -> String
typeName2 :: (Typeable c) => Proxy (c a b) -> String
#else
typeName1 :: (Typeable1 c) => Proxy (c a) -> String
typeName2 :: (Typeable2 c) => Proxy (c a b) -> String
#endif

typeName1 proxy = show (typeOf1 (undefined `asProxyType` proxy))
typeName2 proxy = show (typeOf2 (undefined `asProxyType` proxy))

getGenericVector :: (SafeCopy a, VG.Vector v a) => Contained (Get (v a))
getGenericVector = contain $ do n <- get
                                getSafeGet >>= VG.replicateM n

putGenericVector :: (SafeCopy a, VG.Vector v a) => v a -> Contained Put
putGenericVector v = contain $ do put (VG.length v)
                                  getSafePut >>= VG.forM_ v

instance SafeCopy a => SafeCopy (V.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeCopy a, VP.Prim a) => SafeCopy (VP.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeCopy a, VS.Storable a) => SafeCopy (VS.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeCopy a, VU.Unbox a) => SafeCopy (VU.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector
