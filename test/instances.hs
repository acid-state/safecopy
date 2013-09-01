{-# LANGUAGE TemplateHaskell #-}

import Data.List
import Data.SafeCopy
import Data.Serialize
import Language.Haskell.TH
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

do safecopy <- reify ''SafeCopy
   arbitrary' <- reify ''Arbitrary
   eq <- reify ''Eq
   show' <- reify ''Show
   let instances (ClassI _ decs) = [ typ | InstanceD _ (AppT _ typ) _ <- decs ]
       instances _ = []
       types = map instances [safecopy, arbitrary', eq, show']
       common = foldl1 intersect types
       prop typ =
           [| testProperty $(litE $ stringL $ pprint typ) $ \a ->
              let b = (runGet safeGet . runPut . safePut) (a :: $(return typ))
              in printTestCase (show b) $ b == Right a |]
       props = listE $ map prop common
   [d| isomorphisms :: [TestTree]
       isomorphisms = $props |]

main :: IO ()
main = defaultMain $ testGroup "SafeCopy instances"
    [ testGroup "isomorphisms (decode . encode = id)" isomorphisms
    ]
