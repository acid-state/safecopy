{-# LANGUAGE TemplateHaskell #-}
module Data.SafeCopy.Derive (deriveSafeCopy, deriveSafeCopySimple) where

import Data.Binary (getWord8, putWord8)
import Data.SafeCopy.SafeCopy

import Language.Haskell.TH hiding (Kind(..))
import Control.Applicative
import Control.Monad

{-
instance (SafeCopy keys) => SafeCopy (name keys) where
    version = ?version?
    kind = ?kind?
    putCopy (Con a b c) = contain $ ...
    getCopy = contain $ ...
-}
deriveSafeCopy :: Integer -> Name -> Name -> Q [Dec]
deriveSafeCopy = internalDeriveSafeCopy Normal

deriveSafeCopySimple :: Integer -> Name -> Name -> Q [Dec]
deriveSafeCopySimple = internalDeriveSafeCopy Simple

data DeriveType = Normal | Simple

internalDeriveSafeCopy :: DeriveType -> Integer -> Name -> Name -> Q [Dec]
internalDeriveSafeCopy deriveType versionId kindName tyName
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
                                       [ mkPutCopy deriveType cons
                                       , mkGetCopy deriveType tyName cons
                                       , valD (varP 'version) (normalB (litE (integerL versionId))) []
                                       , valD (varP 'kind) (normalB (varE kindName)) []
                                       ]

mkPutCopy :: DeriveType -> [(Integer, Con)] -> DecQ
mkPutCopy deriveType cons = funD 'putCopy $ map mkPutClause cons
    where
      manyConstructors = length cons > 1
      mkPutClause (conNumber, con)
          = do putVars <- replicateM (conSize con) (newName "arg")
               (putFunsDecs, putFuns) <- case deriveType of
                                           Normal -> mkSafeFunctions "safePut_" 'getSafePut con
                                           Simple -> return ([], const 'safePut)
               let putClause   = conP (conName con) (map varP putVars)
                   putCopyBody = varE 'contain `appE` doE (
                                   [ noBindS $ varE 'putWord8 `appE` (litE $ IntegerL conNumber) | manyConstructors ] ++
                                   putFunsDecs ++
                                   [ noBindS $ varE (putFuns typ) `appE` varE var | (typ, var) <- zip (conTypes con) putVars ] ++
                                   [ noBindS $ varE 'return `appE` tupE [] ])
               clause [putClause] (normalB putCopyBody) []

mkGetCopy :: DeriveType -> Name -> [(Integer, Con)] -> DecQ
mkGetCopy deriveType tyName cons = valD (varP 'getCopy) (normalB $ varE 'contain `appE` getCopyBody) []
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
          = do (getFunsDecs, getFuns) <- case deriveType of
                                           Normal -> mkSafeFunctions "safeGet_" 'getSafeGet con
                                           Simple -> return ([], const 'safeGet)
               let getBase = appE (varE 'return) (conE (conName con))
                   getArgs = foldl (\a t -> infixE (Just a) (varE '(<*>)) (Just (varE (getFuns t)))) getBase (conTypes con)
               doE (getFunsDecs ++ [noBindS getArgs])
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

mkSafeFunctions :: String -> Name -> Con -> Q ([StmtQ], Type -> Name)
mkSafeFunctions name baseFun con = finish <$> foldM f ([], []) (conTypes con)
    where f (ds, fs) t
              | any ((== t) . fst) fs = return (ds, fs)
              | otherwise             = do funVar <- newName (name ++ typeName t)
                                           return ( bindS (varP funVar) (varE baseFun) : ds
                                                  , (t, funVar) : fs )
          finish (ds, fs) = (reverse ds, f)
              where f typ = case lookup typ fs of
                              Just f  -> f
                              Nothing -> error "mkSafeFunctions: never here"
    -- We can't use a Data.Map because Type isn't a member of Ord =/...

conSize :: Con -> Int
conSize (NormalC _name args) = length args
conSize (RecC _name recs)    = length recs
conSize InfixC{}             = 2
conSize ForallC{}            = error "Found complex constructor. Cannot derive SafeCopy for it."

conName :: Con -> Name
conName (NormalC name _args) = name
conName (RecC name _recs)    = name
conName (InfixC _ name _)    = name

conTypes :: Con -> [Type]
conTypes (NormalC _name args)       = [t | (_, t)    <- args]
conTypes (RecC _name args)          = [t | (_, _, t) <- args]
conTypes (InfixC (_, t1) _ (_, t2)) = [t1, t2]

typeName :: Type -> String
typeName (VarT name) = nameBase name
typeName (ConT name) = nameBase name
typeName (TupleT n)  = "Tuple" ++ show n
typeName ArrowT      = "Arrow"
typeName ListT       = "List"
typeName (AppT t u)  = typeName t ++ typeName u
typeName (SigT t _k) = typeName t
typeName _           = "_"
