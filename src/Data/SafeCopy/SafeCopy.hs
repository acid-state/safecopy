{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Data.Serialize

import Control.Monad
import Data.Int (Int32)
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

    -- | Internal function that should not be overrided.
    --   @Consistent@ iff the version history is consistent
    --   (i.e. there are no duplicate version numbers) and
    --   the chain of migrations is valid.
    --
    --   This function is in the typeclass so that this
    --   information is calculated only once during the program
    --   lifetime, instead of everytime 'safeGet' or 'safePut' is
    --   used.
    internalConsistency :: Consistency a
    internalConsistency =
        let ret = computeConsistency proxy
            proxy = proxyFromConsistency ret
        in ret



constructGetterFromVersion :: SafeCopy a => Version a -> Proxy a -> Get a
constructGetterFromVersion diskVersion a_proxy
    | version == diskVersion = unsafeUnPack getCopy
    | otherwise              = case kindFromProxy a_proxy of
                                 Primitive -> fail "Cannot migrate from primitive types."
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
getSafeGet :: forall a. SafeCopy a => Get (Get a)
getSafeGet
    = checkConsistency proxy $
      case kindFromProxy proxy of
        Primitive -> return $ unsafeUnPack getCopy
        _         -> do v <- get
                        return $ constructGetterFromVersion v proxy
    where proxy = Proxy :: Proxy a

-- | Serialize a data type by first writing out its version tag. This is much
--   simpler than the corresponding 'safeGet' since previous versions don't
--   come into play.
safePut :: SafeCopy a => a -> Put
safePut a
    = do putter <- getSafePut
         putter a

-- | Serialize the version tag and return the associated putter. This is useful
--   when serializing multiple values with the same version. See 'getSafeGet'.
getSafePut :: forall a. SafeCopy a => PutM (a -> Put)
getSafePut
    = checkConsistency proxy $
      case kindFromProxy proxy of
        Primitive -> return $ \a -> unsafeUnPack (putCopy $ asProxyType a proxy)
        _         -> do put (versionFromProxy proxy)
                        return $ \a -> unsafeUnPack (putCopy $ asProxyType a proxy)
    where proxy = Proxy :: Proxy a

-- | The extension kind lets the system know that there is
--   at least one previous version of this type. A given data type
--   can only extend a single other data type. However, it is
--   perfectly fine to build chains of extensions. The migrations
--   between each step is handled automatically.
extension :: (SafeCopy a, Migrate a) => Kind a
extension = Extends Proxy

-- | The default kind. Does not extend any type.
base :: Kind a
base = Base

-- | Primitive kinds aren't version tagged. This kind is used for small or built-in
--   types that won't change such as 'Int' or 'Bool'.
primitive :: Kind a
primitive = Primitive

-------------------------------------------------
-- Data type versions. Essentially just a unique
-- identifier used to lookup the corresponding
-- parser function.

-- | A simple numeric version id.
newtype Version a = Version {unVersion :: Int32} deriving (Read,Show,Eq)

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

instance Serialize (Version a) where
    get = liftM Version get
    put = put . unVersion

-------------------------------------------------
-- Container type to control the access to the
-- parsers/putters.

-- | To ensure that no-one reads or writes values without handling versions
--   correct, it is necessary to restrict access to 'getCopy' and 'putCopy'.
--   This is where 'Contained' enters the picture. It allows you to put
--   values in to a container but not to take them out again.
newtype Contained a = Contained {unsafeUnPack :: a}

-- | Place a value in an unbreakable container.
contain :: a -> Contained a
contain = Contained

-------------------------------------------------
-- Consistency checking

data Consistency a = Consistent | NotConsistent String

availableVersions :: SafeCopy a => Proxy a -> [Int32]
availableVersions a_proxy
    = case kindFromProxy a_proxy of
        Primitive -> []
        Base      -> [unVersion (versionFromProxy a_proxy)]
        Extends b_proxy ->unVersion (versionFromProxy a_proxy) : availableVersions b_proxy

-- Extend chains must end in a Base kind. Ending in a Primitive is an error.
validChain :: SafeCopy a => Proxy a -> Bool
validChain a_proxy
    = case kindFromProxy a_proxy of
        Primitive       -> True
        Base            -> True
        Extends b_proxy -> check b_proxy
    where check :: SafeCopy b => Proxy b -> Bool
          check b_proxy
              = case kindFromProxy b_proxy of
                  Primitive       -> False
                  Base            -> True
                  Extends c_proxy -> check c_proxy

-- Verify that the SafeCopy instance is consistent.
checkConsistency :: (SafeCopy a, Monad m) => Proxy a -> m b -> m b
checkConsistency proxy ks
    = case consistentFromProxy proxy of
        NotConsistent msg -> fail msg
        Consistent        -> ks

{-# INLINE computeConsistency #-}
computeConsistency :: SafeCopy a => Proxy a -> Consistency a
computeConsistency proxy
    -- Match a few common cases before falling through to the general case.
    -- This allows use to generate nearly all consistencies at compile-time.
    | isObviouslyConsistent (kindFromProxy proxy)
    = Consistent
    | versions /= nub versions
    = NotConsistent $ "Duplicate version tags: " ++ show versions
    | not (validChain proxy)
    = NotConsistent "Primitive types cannot be extended as they have no version tag."
    | otherwise
    = Consistent
    where versions = availableVersions proxy

isObviouslyConsistent :: Kind a -> Bool
isObviouslyConsistent Primitive = True
isObviouslyConsistent Base      = True
isObviouslyConsistent _         = False

-------------------------------------------------
-- Small utility functions that mean we don't
-- have to depend on ScopedTypeVariables.

proxyFromConsistency :: Consistency a -> Proxy a
proxyFromConsistency _ = Proxy

consistentFromProxy :: SafeCopy a => Proxy a -> Consistency a
consistentFromProxy _ = internalConsistency

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
