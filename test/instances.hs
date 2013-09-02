{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Language.Haskell.TH.PprLib
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding (Fixed)
import Text.PrettyPrint

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f) =>
         Arbitrary (a,b,c,d,e,f) where
   arbitrary = (,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*>
                           arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g) =>
         Arbitrary (a,b,c,d,e,f,g) where
   arbitrary = (,,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*>
                            arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

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

       renderOneLine = renderStyle style { mode = OneLineMode }
       name = renderOneLine . to_HPJ_Doc . ppr
       shorten s | length s > 69 = take 69 s ++ "…"
                 | otherwise     = s

       prop typ =
           [| testProperty $(litE . stringL . shorten $ name typ)
               ($(downsize typ) (prop_inverse :: $(return typ) -> Property)) |]

       props = listE . map prop

   mapM_ (\typ -> reportWarning $ "not tested: " ++ name typ) untested

   [d| inversions :: [TestTree]
       inversions = $(props included) ++ $(props exclusive) |]

main :: IO ()
main = defaultMain $ testGroup "SafeCopy instances"
    [ testGroup "decode is the inverse of encode" inversions
    ]
