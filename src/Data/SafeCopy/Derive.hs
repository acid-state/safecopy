{-# LANGUAGE TemplateHaskell #-}
module Data.SafeCopy.Derive where

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
           TyConI (DataD cxt _name tyvars [con] _derivs)
             -> worker cxt tyvars con
           TyConI DataD{}
             -> fail $ "Can't derive SafeCopy instance for: " ++ show tyName ++
                       ". The datatype must have exactly one constructor."
           TyConI (NewtypeD cxt _name tyvars con _derivs)
             -> worker cxt tyvars con
           _ -> fail $ "Can't derive SafeCopy instance for: " ++ show (tyName, info)
    where worker context tyvars con
              = do putCopyDec <- mkPutCopy con
                   getCopyDec <- mkGetCopy con
                   let ty = foldl appT (conT tyName) [ varT var | PlainTV var <- tyvars ]
                   i <- instanceD (cxt $ [classP ''SafeCopy [varT var] | PlainTV var <- tyvars] ++ map return context)
                                  (conT ''SafeCopy `appT` ty)
                                  [ putCopyDec
                                  , getCopyDec
                                  , valD (varP 'version) (normalB (litE (integerL versionId))) []
                                  , valD (varP 'kind) (normalB (varE kindName)) []
                                  ]
                   return [i]
          mkPutCopy con
              = do putVars <- replicateM (conSize con) (newName "arg")
                   let putClause   = conP (conName con) (map varP putVars)
                       putCopyBody = varE 'contain `appE` doE ( [ noBindS $ varE 'safePut `appE` varE var | var <- putVars ] ++
                                                                [ noBindS $ varE 'return `appE` tupE [] ])
                   return $ funD 'putCopy [clause [putClause] (normalB putCopyBody) []]
          mkGetCopy con
              = do let getBase     = appE (varE 'return) (conE (conName con))
                       getArgs     = foldl (\a b -> infixE (Just a) b (Just (varE 'safeGet))) getBase (replicate (conSize con) (varE '(<*>)))
                       getCopyBody = varE 'contain `appE` getArgs
                   return $ valD (varP 'getCopy) (normalB getCopyBody) []
          conSize (NormalC _name args) = length args
          conSize (RecC _name recs) = length recs
          conSize InfixC{} = 2
          conSize ForallC{} = error "Found complex constructor. Cannot derive SafeCopy for it."
          conName (NormalC name _args) = name
          conName (RecC name _recs) = name
          conName (InfixC _ name _) = name


