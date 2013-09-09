{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Hack for bug in older Cabal versions
#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) 1
#endif

import Control.Applicative
import Control.Lens
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Data.Lens
import Data.Fixed (Fixed, E1)
import Data.List
import Data.SafeCopy
import Data.Serialize (runPut, runGet)
import Data.Time (UniversalTime(..), ZonedTime(..))
import Data.Tree (Tree)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding (Fixed)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f) =>
         Arbitrary (a,b,c,d,e,f) where
   arbitrary = (,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*>
                           arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g) =>
         Arbitrary (a,b,c,d,e,f,g) where
   arbitrary = (,,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*>
                            arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (V.Vector a) where
   arbitrary = V.fromList <$> arbitrary

instance (Arbitrary a, VP.Prim a) => Arbitrary (VP.Vector a) where
   arbitrary = VP.fromList <$> arbitrary

instance (Arbitrary a, VS.Storable a) => Arbitrary (VS.Vector a) where
   arbitrary = VS.fromList <$> arbitrary

instance (Arbitrary a, VU.Unbox a) => Arbitrary (VU.Vector a) where
   arbitrary = VU.fromList <$> arbitrary

deriving instance (Arbitrary a) => Arbitrary (Prim a)
deriving instance (Eq a) => Eq (Prim a)
deriving instance (Show a) => Show (Prim a)

deriving instance Eq ZonedTime
deriving instance Show UniversalTime

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
   classes <- mapM reify [ name | ClassP name _ <- preds ]
   def <- a

   let instances (ClassI _ decs) = [ typ | InstanceD _ (AppT _ typ) _ <- decs ]
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

#if !MIN_VERSION_template_haskell(2,8,0)
       -- 'report' throws warnings in template-haskell-2.8.0.0
       let reportWarning = report False
#endif

   mapM_ (\typ -> reportWarning $ "not tested: " ++ name typ) untested

   [d| inversions :: [TestTree]
       inversions = $(props included) ++ $(props exclusive) |]

main :: IO ()
main = defaultMain $ testGroup "SafeCopy instances"
    [ testGroup "decode is the inverse of encode" inversions
    ]
