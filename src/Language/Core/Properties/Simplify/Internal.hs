{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Properties.Simplify.Internal where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.MatchBind ( match )
import Language.Core.Properties.TypeOf
import Language.Core.Properties.DomainOf
import Language.Core.Properties.ShowAST
import Language.Core.Properties.Pretty


class Simplify a where
    simplify :: (Functor m, Monad m) => a -> WriterT Any (CompT m) Core

instance Simplify Core where

    simplify p@(L {}) = return p
    simplify p@(R {}) = return p

    -- simplify (Expr ":negate" [Expr ":value" [Expr ":value-literal" [L (I i)]]])
    --     = do
    --         tell $ Any True
    --         return $ Expr ":value" [Expr ":value-literal" [L $ I $ negate i]]

    simplify ( viewDeep [":metavar"] -> Just [R x] ) = simplify ("@" `mappend` x)

    simplify  p@( viewDeep [":operator-hastype"] -> Just [a,b] ) = do
        lift $ mkLog "simplify" $ pretty p
        ta   <- lift $ typeOf a
        tb   <- lift $ typeOf b
        flag <- lift $ typeUnify ta tb
        tell $ Any True
        return $ L $ B flag
    simplify  p@( viewDeep [":operator-hasdomain"] -> Just [a,b] ) = do
        lift $ mkLog "simplify" $ pretty p
        da   <- lift $ domainOf a
        db   <- lift $ domainOf b
        flag <- lift $ domainUnify da db
        tell $ Any True
        return $ L $ B flag

    simplify _p@( viewDeep [":operator-\\/"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (B True)]]
                           , _
                           ]
                 ) = returnTrue
    simplify _p@( viewDeep [":operator-\\/"]
                   -> Just [ _
                           , Expr ":value" [Expr ":value-literal" [L (B True)]]
                           ]
                 ) = returnTrue

    simplify _p@( viewDeep [":operator-\\/"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (B False)]]
                           , x
                           ]
                 ) = tell (Any True) >> return x
    simplify _p@( viewDeep [":operator-\\/"]
                   -> Just [ x
                           , Expr ":value" [Expr ":value-literal" [L (B False)]]
                           ]
                 ) = tell (Any True) >> return x


    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ Expr ":empty-guard" []
                           , x
                           ]
                 ) = tell (Any True) >> return x
    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ x
                           , Expr ":empty-guard" []
                           ]
                 ) = tell (Any True) >> return x

    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (B True)]]
                           , x
                           ]
                 ) = tell (Any True) >> return x
    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ x
                           , Expr ":value" [Expr ":value-literal" [L (B True)]]
                           ]
                 ) = tell (Any True) >> return x

    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (B False)]]
                           ,_
                           ]
                 ) = returnFalse
    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ _
                           , Expr ":value" [Expr ":value-literal" [L (B False)]]
                           ]
                 ) = returnFalse

    simplify _p@( viewDeep [":operator-not"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (B b)]]
                           ]
                 ) = do
                     tell (Any True)
                     return $ valueBool $ not b


    -- simplify _p@( viewDeep [":operator-\\/"] -> Just [a,b] ) = do
    --     a' <- simplify a
    --     b' <- simplify b
    --     return $ Expr ":operator-\\/" [a',b']
    simplify _p@( viewDeep [":operator-="] -> Just [R a,R b] ) | a == b = do
        tell $ Any True
        returnTrue
    simplify _p@( viewDeep [":operator-="] -> Just [ Expr ":value" [Expr ":value-literal" [L a]]
                                                   , Expr ":value" [Expr ":value-literal" [L b]]
                                                   ] ) = do
        tell $ Any True
        return $ valueBool $ a == b

    -- simplify  p@( viewDeep [":operator-="] -> Just [a,b] ) = do
    --     a' <- simplify a
    --     b' <- simplify b
    --             return $ Expr ":operator-=" [a',b']

    simplify _p@( viewDeep [":expr-quantified"] -> Just xs )
        | Just [ R quantifier           ] <- lookUpInExpr ":expr-quantified-quantifier"   xs
        , Just [ Expr ":operator-in" [] ] <- lookUpInExpr ":expr-quantified-quanOverOp"   xs
        , Just [ Expr ":value"
               [ Expr ":value-set" vs
               ]]                         <- lookUpInExpr ":expr-quantified-quanOverExpr" xs
        , Just [ Expr ":structural-single" [qnVar]
               ]                          <- lookUpInExpr ":expr-quantified-quanVar"      xs
        , Just [ qnGuard ]                <- lookUpInExpr ":expr-quantified-guard"        xs
        , Just [ qnBody  ]                <- lookUpInExpr ":expr-quantified-body"         xs
        -- = error $ show qnVar
        = do
            tell $ Any True
            let
                guardOp (Expr ":empty-guard" []) b = return b
                guardOp a b = case quantifier of
                                "forAll" -> return $ Expr ":operator-->"  [a, b]
                                "exists" -> return $ Expr ":operator-/\\" [a, b]
                                "sum"    -> return $ Expr ":operator-*"   [a, b]
                                _        -> err $ "unknown quantifier in simplify" <+> pretty quantifier
                glueOp a b = case quantifier of
                                "forAll" -> return $ Expr ":operator-/\\" [a, b]
                                "exists" -> return $ Expr ":operator-\\/" [a, b]
                                "sum"    -> return $ Expr ":operator-+"   [a, b]
                                _        -> err $ "unknown quantifier in simplify" <+> pretty quantifier
                identity = case quantifier of
                                "forAll" -> return valueTrue
                                "exists" -> return valueFalse
                                "sum"    -> return (valueInt 0)
                                _        -> err $ "unknown quantifier in simplify" <+> pretty quantifier

            identity' <- identity
            vs' <- sequence [ guardOp (replaceCore qnVar v qnGuard)
                                      (replaceCore qnVar v qnBody)
                            | v <- vs ]
            foldM glueOp identity' vs'

    simplify p@(Expr t xs) = do
        ys <- mapM simplify xs
        let result = Expr t ys
        lift $ mkLog "simplify-generic-case"
             $ "generic case:" <++> vcat [ pretty p
                                         , pretty result
                                         ]
        return result
    -- simplify x = do
    --     lift $ mkLog "simplify" $ "default case:" <++>
    --                             vcat [ pretty x
    --                                  , showAST x
    --                                  , stringToDoc $ show x
    --                                  ]
    --     return x

instance Simplify Reference where
    simplify r = core <?> "domain check for reference" <+> showAST r
        where
            core = do
                val <- lift $ lookUpRef r
                tell $ Any True
                simplify val


returnTrue :: (Functor m, Monad m) => WriterT Any (CompT m) Core
returnTrue = tell (Any True) >> return valueTrue

returnFalse :: (Functor m, Monad m) => WriterT Any (CompT m) Core
returnFalse = tell (Any True) >> return valueFalse

domainUnify :: Monad m => Core -> Core -> CompT m Bool
domainUnify y x = do
    mkLog "domainUnify" $ pretty x <+> "~~" <++> pretty y
    match x y
