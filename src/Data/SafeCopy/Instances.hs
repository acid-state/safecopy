{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.SafeCopy.Instances where

import Data.SafeCopy.SafeCopy

import           Control.Applicative
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
import           Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), TimeZone(..), ZonedTime(..))
import qualified Data.Tree as Tree
import           Data.Word
import           System.Time (ClockTime(..), TimeDiff(..), CalendarTime(..), Month(..))
import qualified System.Time as OT

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
    getCopy = contain $ fmap Set.fromDistinctAscList safeGet
    putCopy = contain . safePut . Set.toAscList

instance (SafeCopy a, SafeCopy b, Ord a) => SafeCopy (Map.Map a b) where
    getCopy = contain $ fmap Map.fromDistinctAscList safeGet
    putCopy = contain . safePut . Map.toAscList

instance (SafeCopy a) => SafeCopy (IntMap.IntMap a) where
    getCopy = contain $ fmap IntMap.fromDistinctAscList safeGet
    putCopy = contain . safePut . IntMap.toAscList

instance SafeCopy IntSet.IntSet where
    getCopy = contain $ fmap IntSet.fromDistinctAscList safeGet
    putCopy = contain . safePut . IntSet.toAscList

instance (SafeCopy a) => SafeCopy (Sequence.Seq a) where
    getCopy = contain $ fmap Sequence.fromList safeGet
    putCopy = contain . safePut . Foldable.toList

instance (SafeCopy a) => SafeCopy (Tree.Tree a) where
    getCopy = contain $ liftM2 Tree.Node safeGet safeGet
    putCopy (Tree.Node root sub) = contain $ safePut root >> safePut sub


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

instance (IArray.IArray UArray.UArray e, Ix i, SafeCopy e, SafeCopy i) => SafeCopy (UArray.UArray i e) where
    getCopy = iarray_getCopy
    putCopy = iarray_putCopy


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
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e) =>
         SafeCopy (a,b,c,d,e) where
    kind = primitive
    getCopy = contain $ liftM5 (,,,,) safeGet safeGet safeGet safeGet safeGet
    putCopy (a,b,c,d,e) = contain $ safePut a >> safePut b >> safePut c >> safePut d >> safePut e
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e, SafeCopy f) =>
         SafeCopy (a,b,c,d,e,f) where
    kind = primitive
    getCopy = contain $ (,,,,,) <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet
    putCopy (a,b,c,d,e,f) = contain $ safePut a >> safePut b >> safePut c >> safePut d >>
                                      safePut e >> safePut f
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e, SafeCopy f, SafeCopy g) =>
         SafeCopy (a,b,c,d,e,f,g) where
    kind = primitive
    getCopy = contain $ (,,,,,,) <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*>
                                     safeGet <*> safeGet <*> safeGet
    putCopy (a,b,c,d,e,f,g) = contain $ safePut a >> safePut b >> safePut c >> safePut d >>
                                        safePut e >> safePut f >> safePut g


instance SafeCopy Int where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Integer where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Float where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Double where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy L.ByteString where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy B.ByteString where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Char where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Word8 where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Word16 where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Word32 where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Word64 where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Ordering where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Int8 where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Int16 where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Int32 where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Int64 where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance (Integral a, SafeCopy a) => SafeCopy (Ratio a) where
    kind = primitive;
    getCopy   = contain $ do n <- safeGet
                             d <- safeGet
                             return (n % d)
    putCopy r = contain $ do safePut (numerator   r)
                             safePut (denominator r)
instance (HasResolution a, Fractional (Fixed a)) => SafeCopy (Fixed a) where
    kind = primitive
    getCopy   = contain $ fromRational <$> safeGet
    putCopy   = contain . safePut . toRational

instance SafeCopy () where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance SafeCopy Bool where
    kind = primitive; getCopy = contain get; putCopy = contain . put
instance (SafeCopy a, SafeCopy b) => SafeCopy (Either a b) where
    kind = primitive
    getCopy = contain $ do n <- get
                           if n then liftM Right safeGet
                                else liftM Left safeGet
    putCopy (Right a) = contain $ put True >> safePut a
    putCopy (Left a) = contain $ put False >> safePut a

--  instances for 'text' library

instance SafeCopy T.Text where
    kind = base
    getCopy = contain $ T.decodeUtf8 <$> safeGet
    putCopy = contain . safePut . T.encodeUtf8

instance SafeCopy TL.Text where
    kind = base
    getCopy = contain $ TL.decodeUtf8 <$> safeGet
    putCopy = contain . safePut . TL.encodeUtf8

-- instances for 'time' library

instance SafeCopy Day where
    kind = base
    getCopy = contain $ ModifiedJulianDay <$> safeGet
    putCopy = contain . safePut . toModifiedJulianDay

instance SafeCopy DiffTime where
    kind = base
    getCopy = contain $ fromRational <$> safeGet
    putCopy = contain . safePut . toRational

instance SafeCopy UniversalTime where
    kind = base
    getCopy = contain $ ModJulianDate <$> safeGet
    putCopy = contain . safePut . getModJulianDate

instance SafeCopy UTCTime where
    kind = base
    getCopy   = contain $ do day      <- safeGet
                             diffTime <- safeGet
                             return (UTCTime day diffTime)
    putCopy u = contain $ do safePut (utctDay u)
                             safePut (utctDayTime u)

instance SafeCopy NominalDiffTime where
    kind = base
    getCopy = contain $ fromRational <$> safeGet
    putCopy = contain . safePut . toRational

instance SafeCopy TimeOfDay where
    kind = base
    getCopy   = contain $ do hour <- safeGet
                             mins <- safeGet
                             sec  <- safeGet
                             return (TimeOfDay hour mins sec)
    putCopy t = contain $ do safePut (todHour t)
                             safePut (todMin t)
                             safePut (todSec t)

instance SafeCopy TimeZone where
    kind = base
    getCopy   = contain $ do mins       <- safeGet
                             summerOnly <- safeGet
                             zoneName   <- safeGet
                             return (TimeZone mins summerOnly zoneName)
    putCopy t = contain $ do safePut (timeZoneMinutes t)
                             safePut (timeZoneSummerOnly t)
                             safePut (timeZoneName t)

instance SafeCopy LocalTime where
    kind = base
    getCopy   = contain $ do day <- safeGet
                             tod <- safeGet
                             return (LocalTime day tod)
    putCopy t = contain $ do safePut (localDay t)
                             safePut (localTimeOfDay t)

instance SafeCopy ZonedTime where
    kind = base
    getCopy   = contain $ do localTime <- safeGet
                             timeZone  <- safeGet
                             return (ZonedTime localTime timeZone)
    putCopy t = contain $ do safePut (zonedTimeToLocalTime t)
                             safePut (zonedTimeZone t)

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
                             tzname <- get
                             tz     <- safeGet
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
