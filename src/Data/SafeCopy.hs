-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SafeCopy
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  non-portable (uses GHC extensions)
--
-- SafeCopy extends the parsing and serialization capabilities of Data.Binary
-- to include nested version control. Nested version control means that you
-- can change the definition and binary format of a type nested deep within
-- other types without problems.
--
-- Consider this scenario. You want to store your contact list on disk
-- and so write the following code:
--
-- @
--type Name     = String
--type Address  = String
--data Contacts = Contacts [(Name, Address)]
--instance SafeCopy Contacts where
--     putCopy (Contacts list) = contain $ safePut list
--     getCopy = contain $ Contacts \<$\> safeGet
-- @
--
-- At this point, everything is fine. You get the awesome speed of Data.Binary
-- together with Haskell's ease of use. However, things quickly take a U-turn for the worse
-- when you realize that you want to keep phone numbers as well as names and
-- addresses. Being the experienced coder that you are, you see that using a 3-tuple
-- isn't very pretty and you'd rather use a record. At first you fear that this
-- change in structure will invalidate all your old data. Those fears are quickly quelled,
-- though, when you remember how nifty SafeCopy is. With renewed enthusiasm,
-- you set out and write the following code:
--
-- @
--type Name = String
--type Address = String
--type Phone = String
--
--{- We rename our old Contacts structure -}
--data Contacts_v0 = Contacts_v0 [(Name, Address)]
--instance SafeCopy Contacts_v0 where
--     putCopy (Contacts_v0 list) = contain $ safePut list
--     getCopy = contain $ Contacts_v0 \<$\> safeGet
--
--data Contact = Contact { name    :: Name
--                        , address :: Address
--                        , phone   :: Phone }
--instance SafeCopy Contact where
--    putCopy Contact{..} = contain $ do safePut name; safePut address; safePut phone
--    getCopy = contain $ Contact \<$\> safeGet \<*\> safeGet \<*\> safeGet
--
--data Contacts = Contacts [Contact]
--instance SafeCopy Contacts where
--     version = 2
--     kind = extension
--     putCopy (Contacts contacts) = contain $ safePut contacts
--     getCopy = contain $ Contacts \<$\> safeGet
--
--{- Here the magic happens: -}
--instance Migrate Contacts where
--     type MigrateFrom Contacts = Contacts_v0
--     migrate (Contacts_v0 contacts) = Contacts [ Contact{ name    = name
--                                                        , address = address
--                                                        , phone   = \"\" }
--                                               | (name, address) <- contacts ]
-- @
--
-- With this, you reflect on your code and you are happy. You feel confident in the safety of
-- your data and you know you can remove @Contacts_v0@ once you no longer wish to support
-- that legacy format.
module Data.SafeCopy
    (
      safeGet
    , safePut
    , SafeCopy(version, kind, getCopy, putCopy)
    , Migrate(..)
    , Kind
    , extension
    , Contained
    , contain
    , Version

      -- * Template haskell functions
    , deriveSafeCopy
    , deriveSafeCopySimple
    , deriveSafeCopyHappstackData

      -- * Rarely used functions
    , getSafeGet
    , getSafePut
    , primitive
    , base
    ) where

import Data.SafeCopy.Instances
import Data.SafeCopy.SafeCopy
import Data.SafeCopy.Derive
