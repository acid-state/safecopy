{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Applicative
import Control.Lens           (transformOn, transformOnOf)
import Control.Lens.Traversal (Traversal')
import Control.Lens.Action    ((^!!), act)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Data.Lens         (template)
import Data.Fixed (Fixed, E1)
import Data.List
import Data.SafeCopy
import Data.SafeCopy.Internal (renderTH, renderDecs)
import Data.Serialize (runPut, runGet, Serialize)
import Data.Set (Set)
import Data.Time (UniversalTime(..), ZonedTime(..))
import Data.Tree (Tree)
import Data.Typeable (Typeable)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Fixed, (===))
import Types
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

deriving instance (Arbitrary a) => Arbitrary (Prim a)
deriving instance (Eq a) => Eq (Prim a)
deriving instance (Show a) => Show (Prim a)

deriving instance Eq ZonedTime

-- | Equality on the 'Right' value, showing the unequal value on failure;
-- or explicit failure using the 'Left' message without equality testing.
(===) :: (Eq a, Show a) => Either String a -> a -> Property
Left  e === _ = printTestCase e False
Right a === b = printTestCase (show a) $ a == b

-- | An instance for 'SafeCopy' makes a type isomorphic to a bytestring
-- serialization, which is to say that @decode . encode = id@, i.e.
-- @decode@ is the inverse of @encode@ if we ignore bottom.
prop_inverse :: (SafeCopy a, Arbitrary a, Eq a, Show a) => a -> Property
prop_inverse a = (decode . encode) a === a where
    encode = runPut . safePut
    decode = runGet safeGet

-- | Test the 'prop_inverse' property against all 'SafeCopy' instances
-- (that also satisfy the rest of the constraints) defaulting any type
-- variables to 'Int'.
do let a = conT ''Int

   -- types we skip because the Int defaulting doesn't type check
   excluded <- sequence
      [ [t| Fixed $a |]
      ]

   -- instead we include these hand-defaulted types
   included <- sequence
      [ [t| Fixed E1 |]
      ]

   -- types whose samples grow exponentially and need a lower maxSize
   downsized <- sequence
      [ [t| Array $a $a |]
      , [t| UArray $a $a |]
      , [t| Tree $a |]
      ]

   safecopy <- reify ''SafeCopy
   preds <- 'prop_inverse ^!! act reify . (template :: Traversal' Info Pred)
   classes <-
         case preds of
           [ForallT _ cxt' _] ->
              mapM reify [ name | AppT (ConT name) _ <- cxt' ]
           _ -> error "FIXME: fix this code to handle this case."
   def <- a

   let instances (ClassI _ decs) = [ typ | InstanceD _ _ (AppT _ typ) _ <- decs ]
       instances _ = []
       types = map instances classes

       defaulting (VarT _) = def
       defaulting t = t
       defaulted = transformOn (traverse.traverse) defaulting types
       wanted = transformOn traverse defaulting $ instances safecopy

       common = foldl1 intersect defaulted
       untested = wanted \\ common
       exclusive = filter (`notElem` excluded) common

       downsize typ | typ `elem` downsized = [| mapSize (`div` 5) |]
                    | otherwise            = [| id |]

       unqualifying (Name occ _) = Name occ NameS
       name = pprint . transformOnOf template template unqualifying

       prop typ =
           [| testProperty $(litE . stringL $ name typ)
               ($(downsize typ) (prop_inverse :: $(return typ) -> Property)) |]

       props = listE . map prop

   mapM_ (\typ -> reportWarning $ "not tested: " ++ name typ) untested

   [d| inversions :: [TestTree]
       inversions = $(props included) ++ $(props exclusive) |]

main :: IO ()
main = defaultMain $ testGroup "SafeCopy instances"
    [ testGroup "decode is the inverse of encode" inversions
    , deriveTests ]


-- | A smattering of test cases.
deriveTests =
  testGroup "deriveSafeCopy"
    [ testCase "deriveSafeCopy 0 'base ''(,,,,,,,)" $ do
          let decs = $(do addDependentFile "src/Data/SafeCopy/Derive.hs"
                          lift =<< deriveSafeCopy 0 'base ''(,,,,,,,))
          renderDecs decs @?= intercalate "\n"
            ["instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e, SafeCopy f, SafeCopy g, SafeCopy h) => SafeCopy ((,,,,,,,) a b c d e f g h)",
             "    where putCopy ((,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8) = contain (do {safePut_a <- getSafePut; safePut_b <- getSafePut; safePut_c <- getSafePut; safePut_d <- getSafePut; safePut_e <- getSafePut; safePut_f <- getSafePut; safePut_g <- getSafePut; safePut_h <- getSafePut; safePut_a a1; safePut_b a2; safePut_c a3; safePut_d a4; safePut_e a5; safePut_f a6; safePut_g a7; safePut_h a8; return ()})",
             "          getCopy = contain (label \"GHC.Tuple.(,,,,,,,):\" (do {safeGet_a <- getSafeGet; safeGet_b <- getSafeGet; safeGet_c <- getSafeGet; safeGet_d <- getSafeGet; safeGet_e <- getSafeGet; safeGet_f <- getSafeGet; safeGet_g <- getSafeGet; safeGet_h <- getSafeGet; (((((((return (,,,,,,,) <*> safeGet_a) <*> safeGet_b) <*> safeGet_c) <*> safeGet_d) <*> safeGet_e) <*> safeGet_f) <*> safeGet_g) <*> safeGet_h}))",
             "          version = 0",
             "          kind = base",
             "          errorTypeName _ = \"GHC.Tuple.(,,,,,,,)\""]
      -- Test passing a type to deriveSafeCopy' instead of a Name.
      , testCase "deriveSafeCopy' 0 'base [t(,,,,,,,)|]" $ do
          let decs = $(do addDependentFile "src/Data/SafeCopy/Derive.hs"
                          lift =<< deriveSafeCopy' 0 'base [t|forall a b c d e f g h. (Show a, Typeable a) => (a,b,c,d,e,f,g,h)|])
          renderDecs decs @?= intercalate "\n"
            ["instance (Show a, Typeable a, SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e, SafeCopy f, SafeCopy g, SafeCopy h) => SafeCopy ((,,,,,,,) a b c d e f g h)",
             "    where putCopy ((,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8) = contain (do {safePut_a <- getSafePut; safePut_b <- getSafePut; safePut_c <- getSafePut; safePut_d <- getSafePut; safePut_e <- getSafePut; safePut_f <- getSafePut; safePut_g <- getSafePut; safePut_h <- getSafePut; safePut_a a1; safePut_b a2; safePut_c a3; safePut_d a4; safePut_e a5; safePut_f a6; safePut_g a7; safePut_h a8; return ()})",
             "          getCopy = contain (label \"GHC.Tuple.(,,,,,,,):\" (do {safeGet_a <- getSafeGet; safeGet_b <- getSafeGet; safeGet_c <- getSafeGet; safeGet_d <- getSafeGet; safeGet_e <- getSafeGet; safeGet_f <- getSafeGet; safeGet_g <- getSafeGet; safeGet_h <- getSafeGet; (((((((return (,,,,,,,) <*> safeGet_a) <*> safeGet_b) <*> safeGet_c) <*> safeGet_d) <*> safeGet_e) <*> safeGet_f) <*> safeGet_g) <*> safeGet_h}))",
             "          version = 0",
             "          kind = base",
             "          errorTypeName _ = \"GHC.Tuple.(,,,,,,,)\""]
      -- A type with a type parameter results in some context.
      , testCase "deriveSafeCopy' 0 'base (ClientView db)" $ do
          let decs = $(do addDependentFile "src/Data/SafeCopy/Derive.hs"
                          lift =<< deriveSafeCopy' 0 'base [t|forall db. ClientView db|])
          renderDecs decs @?= intercalate "\n"
            ["instance SafeCopy (ViewModifiers db (PKey Client)) => SafeCopy (ClientView db)",
             "    where putCopy (ClientView a1) = contain (do {safePut_ViewModifiersdbPKeyClient <- getSafePut; safePut_ViewModifiersdbPKeyClient a1; return ()})",
             "          getCopy = contain (label \"Types.ClientView:\" (do {safeGet_ViewModifiersdbPKeyClient <- getSafeGet; return ClientView <*> safeGet_ViewModifiersdbPKeyClient}))",
             "          version = 0",
             "          kind = base",
             "          errorTypeName _ = \"Types.ClientView\""]
      -- There is a hidden forall type parameter inside SearchTerm, so
      -- context is generated here.
      , testCase "deriveSafeCopy' 0 'base SearchTerm" $ do
          let decs = $(do addDependentFile "src/Data/SafeCopy/Derive.hs"
                          lift =<< deriveSafeCopy' 0 'base [t|SearchTerm|])
          renderDecs decs @?= intercalate "\n"
            ["instance SafeCopy SearchTerm",
             "    where putCopy (SearchTerm a1) = contain (do {putWord8 0; safePut_ListaListChar <- getSafePut; safePut_ListaListChar a1; return ()})",
             "          putCopy (NoTerm) = contain (do {putWord8 1; return ()})",
             "          getCopy = contain (label \"Types.SearchTerm:\" (do {tag <- getWord8;",
             "                                                            case tag of",
             "                                                                0 -> do {safeGet_ListaListChar <- getSafeGet; return SearchTerm <*> safeGet_ListaListChar}",
             "                                                                1 -> do return NoTerm",
             "                                                                _ -> fail (\"Could not identify tag \\\"\" ++ (show tag ++ \"\\\" for type \\\"Types.SearchTerm\\\" that has only 2 constructors.  Maybe your data is corrupted?\"))}))",
             "          version = 0",
             "          kind = base",
             "          errorTypeName _ = \"Types.SearchTerm\""]
    ]
