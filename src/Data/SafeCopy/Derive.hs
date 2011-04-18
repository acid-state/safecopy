{-# LANGUAGE TemplateHaskell #-}
module Data.SafeCopy.Derive (deriveSafeCopy) where

import Data.Binary (Get, getWord8, putWord8)
import Data.SafeCopy.SafeCopy

import Language.Haskell.TH hiding (Kind(..))
import Control.Applicative
import Control.Monad

{-
instance (SafeCopy keys) => SafeCopy (name keys) where
    version = ?version?
    kind = ?kind?
    putCopy (Con a b c) = contain $ do safePut a; safePut b; safePut c; return ()
    getCopy = contain $ Con <$> safeGet <*> safeGet <*> safeGet
-}
deriveSafeCopy :: Integer -> Name -> Name -> Q [Dec]
deriveSafeCopy versionId kindName tyName
    = do info <- reify tyName
         case info of
           TyConI (DataD cxt _name tyvars cons _derivs)
             | length cons > 255 -> fail $ "Can't derive SafeCopy instance for: " ++ show tyName ++
                                           ". The datatype must have less than 256 constructors."
             | otherwise         -> worker cxt tyvars (zip [0..] cons)
           TyConI (NewtypeD cxt _name tyvars con _derivs)
             -> worker cxt tyvars [(0, con)]
           _ -> fail $ "Can't derive SafeCopy instance for: " ++ show (tyName, info)
    where worker context tyvars cons
              = let ty = foldl appT (conT tyName) [ varT var | PlainTV var <- tyvars ]
                in (:[]) <$> instanceD (cxt $ [classP ''SafeCopy [varT var] | PlainTV var <- tyvars] ++ map return context)
                                       (conT ''SafeCopy `appT` ty)
                                       [ mkPutCopy cons
                                       , mkGetCopy tyName cons
                                       , valD (varP 'version) (normalB (litE (integerL versionId))) []
                                       , valD (varP 'kind) (normalB (varE kindName)) []
                                       ]

mkPutCopy :: [(Integer, Con)] -> DecQ
mkPutCopy cons = funD 'putCopy $ map mkPutClause cons
    where
      manyConstructors = length cons > 1
      mkPutClause (conNumber, con)
          = do putVars <- replicateM (conSize con) (newName "arg")
               let putClause   = conP (conName con) (map varP putVars)
                   putCopyBody = varE 'contain `appE` doE (
                                   [ noBindS $ varE 'putWord8 `appE` (litE $ IntegerL conNumber) | manyConstructors ] ++
                                   [ noBindS $ varE 'safePut `appE` varE var | var <- putVars ] ++
                                   [ noBindS $ varE 'return `appE` tupE [] ])
               clause [putClause] (normalB putCopyBody) []

mkGetCopy :: Name -> [(Integer, Con)] -> DecQ
mkGetCopy tyName cons = valD (varP 'getCopy) (normalB $ varE 'contain `appE` getCopyBody) []
    where
      getCopyBody
          = case cons of
              [(_, con)] -> mkGetBody con
              _          -> do
                tagVar <- newName "tag"
                doE [ bindS (varP tagVar) (varE 'getWord8)
                    , noBindS $ caseE (varE tagVar) (
                        [ match (litP $ IntegerL i) (normalB $ mkGetBody con) [] | (i, con) <- cons ] ++
                        [ match wildP (normalB $ varE 'fail `appE` errorMsg tagVar) [] ]) ]
      mkGetBody con
          = let getBase = appE (varE 'return) (conE (conName con))
            in foldl (\a b -> infixE (Just a) b (Just (varE 'safeGet))) getBase (replicate (conSize con) (varE '(<*>)))
      errorMsg tagVar = infixE (Just $ strE str1) (varE '(++)) $ Just $
                        infixE (Just tagStr) (varE '(++)) (Just $ strE str2)
          where
            strE = litE . StringL
            tagStr = varE 'show `appE` varE tagVar
            str1 = "Could not identify tag \""
            str2 = concat [ "\" for type "
                          , show tyName
                          , " that has only "
                          , show (length cons)
                          , " constructors.  Maybe your data is corrupted?" ]

conSize :: Con -> Int
conSize (NormalC _name args) = length args
conSize (RecC _name recs)    = length recs
conSize InfixC{}             = 2
conSize ForallC{}            = error "Found complex constructor. Cannot derive SafeCopy for it."

conName :: Con -> Name
conName (NormalC name _args) = name
conName (RecC name _recs)    = name
conName (InfixC _ name _)    = name


