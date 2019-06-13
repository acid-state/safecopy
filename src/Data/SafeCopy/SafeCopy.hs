{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE CPP #-}
#ifdef DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures #-}
#endif

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

-- | This is a wrapper type used migrating backwards in the chain of compatible types.
newtype Reverse a = Reverse { unReverse :: a }

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
    Extended  :: (Migrate (Reverse a)) => Kind a -> Kind a

isPrimitive :: Kind a -> Bool
isPrimitive Primitive = True
isPrimitive _ = False

-- | Wrapper for data that was saved without a version tag.
newtype Prim a = Prim { getPrimitive :: a }

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
    internalConsistency = computeConsistency Proxy

    -- | Version profile.
    objectProfile :: Profile a
    objectProfile = mkProfile Proxy

    -- | The name of the type. This is only used in error
    -- message strings.
    -- Feel free to leave undefined in your instances.
    errorTypeName :: Proxy a -> String
    errorTypeName _ = "<unknown type>"

#ifdef DEFAULT_SIGNATURES
    default getCopy :: Serialize a => Contained (Get a)
    getCopy = contain get

    default putCopy :: Serialize a => a -> Contained Put
    putCopy = contain . put
#endif


-- constructGetterFromVersion :: SafeCopy a => Version a -> Kind (MigrateFrom (Reverse a)) -> Get (Get a)
constructGetterFromVersion :: SafeCopy a => Version a -> Kind a -> Either String (Get a)
constructGetterFromVersion diskVersion orig_kind =
  worker False diskVersion orig_kind
  where
    worker :: forall a. SafeCopy a => Bool -> Version a -> Kind a -> Either String (Get a)
    worker fwd thisVersion thisKind
      | version == thisVersion = return $ unsafeUnPack getCopy
      | otherwise =
        case thisKind of
          Primitive -> Left $ errorMsg thisKind "Cannot migrate from primitive types."
          Base      -> Left $ errorMsg thisKind versionNotFound
          Extends b_proxy -> do
            previousGetter <- worker fwd (castVersion diskVersion) (kindFromProxy b_proxy)
            return $ fmap migrate previousGetter
          Extended{} | fwd -> Left $ errorMsg thisKind versionNotFound
          Extended a_kind -> do
            let rev_proxy :: Proxy (MigrateFrom (Reverse a))
                rev_proxy = Proxy
                forwardGetter :: Either String (Get a)
                forwardGetter  = fmap (fmap (unReverse . migrate)) $ worker True (castVersion thisVersion) (kindFromProxy rev_proxy)
                previousGetter :: Either String (Get a)
                previousGetter = worker fwd (castVersion thisVersion) a_kind
            case forwardGetter of
              Left{}    -> previousGetter
              Right val -> Right val
    versionNotFound   = "Cannot find getter associated with this version number: " ++ show diskVersion
    errorMsg fail_kind msg =
        concat
         [ "safecopy: "
         , errorTypeName (proxyFromKind fail_kind)
         , ": "
         , msg
         ]

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
        a_kind    -> do v <- get
                        case constructGetterFromVersion v a_kind of
                          Right getter -> return getter
                          Left msg     -> fail msg
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

-- | The extended_extension kind lets the system know that there is
--   at least one previous and one future version of this type.
extended_extension :: (Migrate a, Migrate (Reverse a)) => Kind a
extended_extension = Extended extension

-- | The extended_base kind lets the system know that there is
--   at least one future version of this type.
extended_base :: (Migrate (Reverse a)) => Kind a
extended_base = Extended base

-- | The extension kind lets the system know that there is
--   at least one previous version of this type. A given data type
--   can only extend a single other data type. However, it is
--   perfectly fine to build chains of extensions. The migrations
--   between each step is handled automatically.
extension :: Migrate a => Kind a
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

data Profile a =
  PrimitiveProfile |
  InvalidProfile String |
  Profile
  { profileCurrentVersion :: Int32
  , profileSupportedVersions :: [Int32]
  } deriving (Show)

mkProfile :: SafeCopy a => Proxy a -> Profile a
mkProfile a_proxy =
  case computeConsistency a_proxy of
    NotConsistent msg -> InvalidProfile msg
    Consistent | isPrimitive (kindFromProxy a_proxy) -> PrimitiveProfile
    Consistent ->
      Profile{ profileCurrentVersion    = unVersion (versionFromProxy a_proxy)
             , profileSupportedVersions = availableVersions a_proxy
             }

data Consistency a = Consistent | NotConsistent String

availableVersions :: SafeCopy a => Proxy a -> [Int32]
availableVersions a_proxy =
  worker True (kindFromProxy a_proxy)
  where
    worker :: SafeCopy b => Bool -> Kind b -> [Int32]
    worker fwd b_kind =
      case b_kind of
        Primitive         -> []
        Base              -> [unVersion (versionFromKind b_kind)]
        Extends b_proxy   -> unVersion (versionFromKind b_kind) : worker False (kindFromProxy b_proxy)
        Extended sub_kind | fwd  -> worker False (getForwardKind sub_kind)
        Extended sub_kind -> worker False sub_kind

getForwardKind :: (Migrate (Reverse a)) => Kind a -> Kind (MigrateFrom (Reverse a))
getForwardKind _ = kind

-- Extend chains must end in a Base kind. Ending in a Primitive is an error.
validChain :: SafeCopy a => Proxy a -> Bool
validChain a_proxy =
  worker (kindFromProxy a_proxy)
  where
    worker Primitive         = True
    worker Base              = True
    worker (Extends b_proxy) = check (kindFromProxy b_proxy)
    worker (Extended a_kind)   = worker a_kind
    check :: SafeCopy b => Kind b -> Bool
    check b_kind
              = case b_kind of
                  Primitive       -> False
                  Base            -> True
                  Extends c_proxy -> check (kindFromProxy c_proxy)
                  Extended sub_kind   -> check sub_kind

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

proxyFromKind :: Kind a -> Proxy a
proxyFromKind _ = Proxy

consistentFromProxy :: SafeCopy a => Proxy a -> Consistency a
consistentFromProxy _ = internalConsistency

versionFromProxy :: SafeCopy a => Proxy a -> Version a
versionFromProxy _ = version

versionFromKind :: (SafeCopy a) => Kind a -> Version a
versionFromKind _ = version

versionFromReverseKind :: (SafeCopy (MigrateFrom (Reverse a))) => Kind a -> Version (MigrateFrom (Reverse a))
versionFromReverseKind _ = version

kindFromProxy :: SafeCopy a => Proxy a -> Kind a
kindFromProxy _ = kind

-------------------------------------------------
-- Type proxies

data Proxy a = Proxy

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

asProxyType :: a -> Proxy a -> a
asProxyType a _ = a
