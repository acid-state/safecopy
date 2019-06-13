{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE CPP #-}
#ifdef DEFAULT_SIGNATURES
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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

#ifdef DEFAULT_SIGNATURES
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State as State (evalStateT, modify, StateT)
import qualified Control.Monad.Trans.State as State (get)
import Control.Monad.Trans.RWS as RWS (evalRWST, modify, RWST, tell)
import qualified Control.Monad.Trans.RWS as RWS (get)
import Data.Bits (shiftR)
import Data.Map as Map (Map, lookup, insert)
import Data.Set as Set (insert, member, Set)
import Data.Typeable (Typeable, TypeRep, typeOf, typeRep)
import Data.Word (Word8)
import GHC.Generics
import Generic.Data as G (Constructors, gconIndex, gconNum)
import Unsafe.Coerce (unsafeCoerce)
#endif

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
    default putCopy :: (GPutCopy (Rep a) DatatypeInfo, Constructors a) => a -> Contained Put
    putCopy a = (contain . gputCopy (ConstructorInfo (fromIntegral (gconNum @a)) (fromIntegral (gconIndex a))) . from) a

    default getCopy :: (GGetCopy (Rep a) DatatypeInfo, Constructors a) => Contained (Get a)
    getCopy = contain (to <$> ggetCopy (ConstructorCount (fromIntegral (gconNum @a))))

class GPutCopy f p where
    gputCopy :: p -> f p -> Put

instance GPutCopy a p => GPutCopy (M1 D c a) p where
    gputCopy p (M1 a) = gputCopy p a
    {-# INLINE gputCopy #-}

instance (GPutCopy f p, GPutCopy g p) => GPutCopy (f :+: g) p where
    gputCopy p (L1 x) = gputCopy @f p x
    gputCopy p (R1 x) = gputCopy @g p x
    {-# INLINE gputCopy #-}

-- To get the current safecopy behavior we need to emulate the
-- template haskell code here - collect the (a -> Put) values for all
-- the fields and then run them in order.o
instance (GPutFields a p, p ~ DatatypeInfo) => GPutCopy (M1 C c a) p where
    gputCopy p (M1 x) =
      (when (_size p >= 2) (putWord8 (fromIntegral (_code p)))) *>
        -- This is how I tried it first, and it works well but the
        -- result is not the same as deriveSafeCopy.
        -- mconcat (fmap join (gputFields p x))
        -- join (mconcat <$> sequence (fmap snd (gputFields p x)))
      (do putter <- (mconcat . snd) <$> (evalRWST (gputFields p x) () mempty)
          putter)
    {-# INLINE gputCopy #-}

-- | gputFields traverses the fields of a constructor and returns a put
-- for the safecopy versions and a put for the field values.
class GPutFields f p where
    gputFields :: p -> f p -> RWST () [Put] (Set TypeRep) PutM ()

instance (GPutFields f p, GPutFields g p) => GPutFields (f :*: g) p where
    gputFields p (a :*: b) = gputFields p a >> gputFields p b

instance GPutFields f p => GPutFields (M1 S c f) p where
    gputFields p (M1 a) = gputFields p a
    {-# INLINE gputFields #-}

instance (SafeCopy a, Typeable a) => GPutFields (K1 R a) p where
    gputFields _ (K1 a) = do
      getSafePutGeneric putCopy a
    {-# INLINE gputFields #-}

#if 1
-- This corresponds to ggetFields, but does it match deriveSafeCopy?
instance GPutFields U1 p where
    gputFields _ _ =
      return ()
#else
-- This outputs the version tag for (), which is 1.
instance (GPutFields (K1 R ()) p) => GPutFields U1 p where
    gputFields p _ =
      gputFields p (K1 () :: K1 R () p)
#endif
    {-# INLINE gputFields #-}

------------------------------------------------------------------------

class GGetCopy f p where
    ggetCopy :: p -> Get (f a)

-- | The M1 type has a fourth type parameter p:
--
--     newtype M1 i (c :: Meta) (f :: k -> *) (p :: k) = M1 {unM1 :: f p}
--
-- Note that the type of the M1 field is @f p@, so in order to express this
-- type we add a parameter of type p that we can apply to values of type f.
instance (GGetCopy f p, p ~ DatatypeInfo) => GGetCopy (M1 D d f) p where
    ggetCopy p
      | _size p >= 2 = do
          !code <- getWord8
          M1 <$> ggetCopy (ConstructorInfo (_size p) code)
      | otherwise = M1 <$> ggetCopy (ConstructorInfo (_size p) 0)
    {-# INLINE ggetCopy #-}

instance (GGetCopy f p, GGetCopy g p, p ~ DatatypeInfo) => GGetCopy (f :+: g) p where
    ggetCopy p = do
      -- choose the left or right branch of the constructor types
      -- based on whether the code is in the left or right half of the
      -- remaining constructor count.
      let sizeL = _size p `shiftR` 1
          sizeR = _size p - sizeL
      case _code p < sizeL of
        True -> L1 <$> ggetCopy @f (ConstructorInfo sizeL (_code p))
        False -> R1 <$> ggetCopy @g (ConstructorInfo sizeR (_code p - sizeL))

instance GGetFields f p => GGetCopy (M1 C c f) p where
    ggetCopy p = do
      M1 <$> join (evalStateT (ggetFields p) mempty)
    {-# INLINE ggetCopy #-}

-- append constructor fields
class GGetFields f p where
    ggetFields :: p -> StateT (Map TypeRep Int32) Get (Get (f a))

instance (GGetFields f p, GGetFields g p) => GGetFields (f :*: g) p where
    ggetFields p = do
      fgetter <- ggetFields @f p
      ggetter <- ggetFields @g p
      return ((:*:) <$> fgetter <*> ggetter)

instance GGetFields f p => GGetFields (M1 S c f) p where
    ggetFields p = do
      getter <- ggetFields p
      return (M1 <$> getter)
    {-# INLINE ggetFields #-}

instance (SafeCopy a, Typeable a) => GGetFields (K1 R a) p where
    ggetFields _ = do
      getter <- getSafeGetGeneric
      return (K1 <$> getter)
    {-# INLINE ggetFields #-}

instance GGetFields U1 p where
    ggetFields _p = pure (pure U1)
    {-# INLINE ggetFields #-}

data DatatypeInfo =
    ConstructorCount {_size :: Word8}
  | ConstructorInfo {_size :: Word8, _code :: Word8}
  deriving Show

-- | Whereas the other 'getSafeGet' is only run when we know we need a
-- version, this one is run for every field and must decide whether to
-- read a version or not.  It constructs a Map TypeRep Int32 and reads
-- whent he new TypeRep is not in the map.  Assumes Version a ~ Int32.
getSafeGetGeneric ::
  forall a. (SafeCopy a, Typeable a)
  => StateT (Map TypeRep Int32) Get (Get a)
getSafeGetGeneric
    = checkConsistency proxy $
      case kindFromProxy proxy of
        Primitive -> return $ unsafeUnPack getCopy
        a_kind    -> do let rep = typeRep (Proxy :: Proxy a)
                        reps <- State.get
                        v <- maybe (lift get) pure (Map.lookup rep reps)
                        -- This coerce creates an assumption that Version a ~ Int32
                        case constructGetterFromVersion (unsafeCoerce v) a_kind of
                          Right getter -> State.modify (Map.insert rep v) >> return getter
                          Left msg     -> fail msg
    where proxy = Proxy :: Proxy a

-- | This version returns (Put, Put), the collected version tags and
-- the collected serialized fields.  The original 'getSafePut' result
-- type prevents doing this because each fields may have a different
-- type.  Maybe you can show me a better way
getSafePutGeneric ::
  forall a. (SafeCopy a, Typeable a)
  => (a -> Contained Put)
  -> a
  -> RWST () [Put] (Set TypeRep) PutM ()
getSafePutGeneric cput a
    = checkConsistency proxy $
      case kindFromProxy proxy of
        Primitive -> tell [unsafeUnPack (cput $ asProxyType a proxy)]
        _         -> do reps <- RWS.get
                        let typ = typeOf a
                        when (not (member typ reps)) $ do
                          lift (put (versionFromProxy proxy))
                          RWS.modify (Set.insert typ)
                        tell [unsafeUnPack (cput $ asProxyType a proxy)]
    where proxy = Proxy :: Proxy a

type GSafeCopy a = (SafeCopy a, Typeable a, Generic a, GPutCopy (Rep a) DatatypeInfo, Constructors a)

-- | Generic only version of safePut. Instead of calling 'putCopy' it
-- calls 'putCopyDefault', a copy of the implementation of the
-- 'SafeCopy' default method for 'putCopy'.
safePutGeneric :: forall a. GSafeCopy a => a -> Put
safePutGeneric a = do
  putter <- (mconcat . snd) <$> evalRWST (getSafePutGeneric putCopyDefault a) () mempty
  putter

-- | See 'safePutGeneric'.  A copy of the code in the default
-- implementation of the putCopy method.
putCopyDefault :: forall a. GSafeCopy a => a -> Contained Put
putCopyDefault a = (contain . gputCopy (ConstructorInfo (fromIntegral (gconNum @a)) (fromIntegral (gconIndex a))) . from) a
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
