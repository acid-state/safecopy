{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SafeCopy.SafeCopy
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  non-portable (uses GHC extensions)
--
-- SafeCopy extends the parsing and serialization capabilities of Data.Binary
-- to include nested version control. Nested version control means that you
-- can change the defintion and binary format of a type nested deep within
-- other types without problems.
--
module Data.SafeCopy.SafeCopy where

import Data.Binary as B
import Data.Binary.Put as B
import Data.Binary.Get as B
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad
import Control.Applicative
import Data.List


-- | The central mechanism for dealing with version control.
--
--   This type class specifies what data migrations can happen
--   and how they happen.
class SafeCopy (MigrateFrom a) => Migrate a where
    -- | This is the type we're extending. Each type capable of migration can
    --   only extend one other type.
    type MigrateFrom a

    -- | This method specifies how to migrate from the older type to the newer
    --   one. It will never be necessary to use this function manually as it
    --   all taken care of internally in the library.
    migrate :: MigrateFrom a -> a

-- | The kind of a data type determines how it is tagged (if at all).
--
--   Primitives kinds (see 'primitive') are not tagged with a version
--   id and hence cannot be extended later.
--
--   Extensions (see 'extension') tells the system that there exists
--   a previous version of the data type which should be migrated if
--   needed.
--
--   There is also a default kind which is neither primitive nor is
--   an extension of a previous type.
data Kind a where
    Primitive :: Kind a
    Base      :: Kind a
    Extends   :: (Migrate a) => Proxy (MigrateFrom a) -> Kind a

-- | The centerpiece of this library. Defines a version for a data type
--   together with how it should be serialized/parsed.
--
--   Users should define instances of 'SafeCopy' for their types
--   even though 'getCopy' and 'putCopy' can't be used directly.
--   To serialize/parse a data type using 'SafeCopy', see 'safeGet'
--   and 'safePut'.
class SafeCopy a where
    -- | The version of the type.
    --
    --   Only used as a key so it must be unique (this is checked at run-time)
    --   but doesn't have to be sequential or continuous.
    --
    --   The default version is '0'.
    version :: Version a
    version = Version 0

    -- | The kind specifies how versions are dealt with. By default,
    --   values are tagged with their version id and don't have any
    --   previous versions. See 'extension' and the much less used
    --   'primitive'.
    kind :: Kind a
    kind = Base

    -- | This method defines how a value should be parsed without also worrying
    --   about writing out the version tag. This function cannot be used directly.
    --   One should use 'safeGet', instead.
    getCopy  :: Contained (Get a)

    -- | This method defines how a value should be parsed without worrying about
    --   previous versions or migrations. This function cannot be used directly.
    --   One should use 'safeGet', instead.
    putCopy  :: a -> Contained Put


constructGetterFromVersion :: SafeCopy a => Version a -> Proxy a -> Get a
constructGetterFromVersion diskVersion a_proxy
    | version == diskVersion = unsafeUnPack getCopy
    | otherwise              = case kindFromProxy a_proxy of
                                 Primitive -> fail $ "Cannot migrate from primitive types."
                                 Base      -> fail $ "Cannot find getter associated with this version number: " ++ show diskVersion
                                 Extends b_proxy
                                   -> fmap migrate (constructGetterFromVersion (castVersion diskVersion) b_proxy)

-------------------------------------------------
-- The public interface. These functions are used
-- to parse/serialize and to create new parsers &
-- serialisers.

-- | Parse a version tagged data type and then migrate it to the desired type.
--   Any serialized value has been extended by the return type can be parsed.
safeGet :: SafeCopy a => Get a
safeGet
    = join getSafeGet

-- | Parse a version tag and return the corresponding migrated parser. This is
--   useful when you can prove that multiple values have the same version.
--   See 'getSafePut'.
getSafeGet :: SafeCopy a => Get (Get a)
getSafeGet
    = checkInvariants proxy $
      case kindFromProxy proxy of
        Primitive -> return $ unsafeUnPack getCopy
        _         -> do v <- get
                        return $ constructGetterFromVersion v proxy
    where proxy = Proxy

-- | Serialize a data type by first writing out its version tag. This is much
--   simpler than the corresponding 'safeGet' since previous versions don't
--   come into play.
safePut :: SafeCopy a => a -> Put
safePut a
    = do putter <- getSafePut
         putter a

-- | Serialize the version tag and return the associated putter. This is useful
--   when serializing multiple values with the same version. See 'getSafeGet'.
getSafePut :: SafeCopy a => PutM (a -> Put)
getSafePut
    = checkInvariants proxy $
      case kindFromProxy proxy of
        Primitive -> return $ \a -> unsafeUnPack (putCopy $ asProxyType a proxy)
        _         -> do put (versionFromProxy proxy)
                        return $ \a -> unsafeUnPack (putCopy $ asProxyType a proxy)
    where proxy = Proxy

-- | The extension kind lets the system know that there is
--   at least one previous version of this type. A given data type
--   can only extend a single other data type. However, it is 
--   perfectly fine to build chains of extensions. The migrations
--   between each step is handled automatically.
extension :: (SafeCopy a, Migrate a) => Kind a
extension = Extends Proxy

-- | Primitive kinds aren't version tagged. This kind is used for small or built-in
--   types that won't change such as 'Int' or 'Bool'.
primitive :: Kind a
primitive = Primitive

-------------------------------------------------
-- Data type versions. Essentially just a unique
-- identifier used to lookup the corresponding
-- parser function.

-- | A simple numeric version id.
newtype Version a = Version {unVersion :: Int} deriving (Read,Show,Eq)

castVersion :: Version a -> Version b
castVersion (Version a) = Version a

instance Num (Version a) where
    Version a + Version b = Version (a+b)
    Version a - Version b = Version (a-b)
    Version a * Version b = Version (a*b)
    negate (Version a) = Version (negate a)
    abs (Version a) = Version (abs a)
    signum (Version a) = Version (signum a)
    fromInteger i = Version (fromInteger i)

instance Binary (Version a) where
    get = liftM Version get
    put = put . unVersion

-------------------------------------------------
-- Container type to control the access to the
-- parsers/putters.

-- | To ensure that no-one reads or writes values without handling versions
--   correct, it is necessary to restrict access to 'getCopy' and 'putCopy'.
--   This is where 'Contained' enters the picture. It allows you to put
--   values in to a container but not to take them out again.
data Contained a = Contained {unsafeUnPack :: a}

-- | Place a value in an unbreakable container.
contain :: a -> Contained a
contain = Contained

-------------------------------------------------
-- Consistency checking

availableVersions :: SafeCopy a => Proxy a -> [Int]
availableVersions a_proxy
    = case kindFromProxy a_proxy of
        Primitive -> []
        Base      -> [unVersion (versionFromProxy a_proxy)]
        Extends b_proxy ->unVersion (versionFromProxy a_proxy) : availableVersions b_proxy

checkInvariants :: (SafeCopy a, Monad m) => Proxy a -> m b -> m b
checkInvariants proxy ks
    = if versions == nub versions
      then ks
      else fail $ "Duplicate version tags: " ++ show versions
    where versions = availableVersions proxy

-------------------------------------------------
-- Small utility functions that mean we don't
-- have to depend on ScopedTypeVariables.

versionFromProxy :: SafeCopy a => Proxy a -> Version a
versionFromProxy _ = version

kindFromProxy :: SafeCopy a => Proxy a -> Kind a
kindFromProxy _ = kind

-------------------------------------------------
-- Type proxies

data Proxy a = Proxy

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

asProxyType :: a -> Proxy a -> a
asProxyType a _ = a
