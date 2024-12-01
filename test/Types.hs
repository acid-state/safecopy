{-# LANGUAGE DataKinds, DeriveDataTypeable, FlexibleInstances, KindSignatures #-}

module Types where

import Control.Monad (MonadPlus, msum)
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Generics (listify)
import Data.Set (Set)
import Data.Typeable (Typeable)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax

newtype ClientView db = ClientView {unClientView :: ViewModifiers db (PKey Client)} deriving (Eq, Ord, Show)
data Client
  = Client
    { _clientName :: PersonName
    , _clientTitle :: RenderedUnicode
    , _clientCompany :: RenderedUnicode
    , _clientAddress :: RenderedUnicode
    , _clientAddress2 :: RenderedUnicode
    , _clientCity :: RenderedUnicode
    , _clientState :: RenderedUnicode
    , _clientPostal :: RenderedUnicode
    , _clientGreeting :: RenderedUnicode
    , _clientPhone :: RenderedUnicode
    , _clientEmail :: RenderedUnicode
    , _clientPreferredContactMethods :: Set ContactMethod
    , _clientNotes :: RenderedHtml
    , _clientUserId :: Maybe UserId
      -- ^ Does this client have have an AppraisalScribe account?  I hope so!
    } deriving (Show, Eq, Ord, Typeable, Data)
type PersonName = String
type RenderedUnicode = String
type RenderedHtml = String
type ContactMethod = Char
newtype UserId = UserId Integer deriving (Eq, Ord, Show, Data)
data ViewModifiers db col = ViewModifiers {_mods :: Set (ViewModifier db)} deriving (Show, Eq, Ord)
data ViewModifier db =
    Effective UserId -- ^ Use this user's data
  | Matching SearchTerm -- ^ Filter out object that do not match this search term
  | PagerView PagerStyle
  | MemberAny [PKey db]
  | ViewNotes [String]
  | InvertMod (ViewModifier db) -- invert the sense of a test
  | TestKeys [ByteString] -- ^ Test whether the key argument matches any encoded key
  deriving (Eq, Ord, Show)
data SearchTerm =
    SearchTerm {_searchWords :: [CI Text]}
  | NoTerm
  deriving (Read, Show, Eq, Ord, Data, Typeable)
type Text = String
type PagerStyle = String
type CI a = a
data OpticTag = F | G | I | L | P | S | T | U deriving (Eq, Show, Typeable)
newtype Path (o :: OpticTag) s = Path UPath deriving (Typeable, Eq, Ord, Data, Show)
type UPath = [UHop Int]
data UHop pos
  = IxPathU ByteString -- ^ 'Control.Lens.ix' path
  | AtPathU ByteString -- ^ 'Control.Lens.at' path
  | NonPathU ByteString -- ^ 'Control.Lens.non' path
  | FieldU pos pos -- ^ Path from a record to one of its fields
  | CtorU pos -- ^ Path from a record to a tuple containing the fields of one of its constructors
  | NewtypePathU -- ^ A path that unwraps a newtype
  | OrderedPathU -- ^ A path to a 'Order'.
  | ViewPathU -- ^ A path corresponding to the 'Control.Lens.Path._View' iso
  | FoldPathU -- ^ A path to an instance of FoldableWithIndex
  | SingularPathU [UHop pos] -- ^ 'Control.Lens.singular' path
  | UnsafeSingularPathU [UHop pos] -- ^ 'Control.Lens.unsafeSingular' path
  deriving (Eq, Ord, Data, Typeable, Show)
newtype PKey s = PKey {unPKey :: Path 'U s} deriving (Typeable, Eq, Ord, Data, Show)


data N a = N a
instance Show (N Name) where
  show (N (Name o f)) = "(Name (" <> show o <> ") (" <> show f <> "))"

gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)
