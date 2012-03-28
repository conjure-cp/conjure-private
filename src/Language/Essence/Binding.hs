{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Binding where

import Control.Applicative
import Control.Monad.Error ( MonadError )
import Control.Monad.State ( MonadState )
import Data.Generics ( Data )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )

import GenericOps.Core ( NodeTag, Hole, GPlate, gplate, gplateTwo, MatchBind, BindingsMap, addBinding )
import ParsecUtils
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>), (<>), Doc)
import qualified PrintUtils as Pr

import Language.Essence.Identifier
import Language.Essence.Domain
import {-# SOURCE #-} Language.Essence.Expr
import Language.Essence.Lambda
import Language.Essence.QuantifierDecl
import Language.Essence.Type
import Language.Essence.Where



addBinding' :: (MonadError Doc m, MonadState BindingsMap m) => Binding -> m ()
addBinding' b@(Find        (Identifier i) _) = addBinding i b
addBinding' b@(Given       (Identifier i) _) = addBinding i b
addBinding' (LettingType   (Identifier i) j) = addBinding i j
addBinding' (GivenType     (Identifier i) j) = addBinding i j
addBinding' (LettingDomain (Identifier i) j) = addBinding i j
addBinding' (LettingExpr   (Identifier i) j) = addBinding i j
addBinding' (LettingLambda (Identifier i) j) = addBinding i j
addBinding' (LettingQuan   (Identifier i) j) = addBinding i j

bindingName :: Binding -> String
bindingName (Find          (Identifier nm) _) = nm
bindingName (Given         (Identifier nm) _) = nm
bindingName (LettingType   (Identifier nm) _) = nm
bindingName (GivenType     (Identifier nm) _) = nm
bindingName (LettingDomain (Identifier nm) _) = nm
bindingName (LettingExpr   (Identifier nm) _) = nm
bindingName (LettingLambda (Identifier nm) _) = nm
bindingName (LettingQuan   (Identifier nm) _) = nm


data Binding
    = Find          Identifier Domain
    | Given         Identifier Domain
    | LettingType   Identifier Type
    | GivenType     Identifier Type
    | LettingDomain Identifier Domain
    | LettingExpr   Identifier Expr
    | LettingLambda Identifier Lambda
    | LettingQuan   Identifier QuantifierDecl
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Binding

instance Hole Binding

instance GPlate Binding where
    gplate (Find          i j) = gplateTwo Find          i j
    gplate (Given         i j) = gplateTwo Given         i j
    gplate (LettingType   i j) = gplateTwo LettingType   i j
    gplate (GivenType     i j) = gplateTwo GivenType     i j
    gplate (LettingDomain i j) = gplateTwo LettingDomain i j
    gplate (LettingExpr   i j) = gplateTwo LettingExpr   i j
    gplate (LettingLambda i j) = gplateTwo LettingLambda i j
    gplate (LettingQuan   i j) = gplateTwo LettingQuan   i j

instance MatchBind Binding

instance ParsePrint Binding where
    parse = error "do not use this one directly. use it via (parse :: [Binding])"
    pretty (Find          i j) = "find"    <+> pretty i <> Pr.colon <+> pretty j
    pretty (Given         i j) = "given"   <+> pretty i <> Pr.colon <+> pretty j
    pretty (LettingType   i j) = "letting" <+> pretty i <+> "be new type" <+> pretty j
    pretty (GivenType     i j) = "given"   <+> pretty i <> Pr.colon <+> "new type" <+> pretty j
    pretty (LettingDomain i j) = "letting" <+> pretty i <+> "be" <+> "domain" <+> pretty j
    pretty (LettingExpr   i j) = "letting" <+> pretty i <+> "be"              <+> pretty j
    pretty (LettingLambda i j) = "letting" <+> pretty i <+> "be" <+> "lambda" <+> pretty j
    pretty (LettingQuan   i j) = "letting" <+> pretty i <+> "be"              <+> pretty j

instance ParsePrint [Binding] where
    parse = do
        let one = choiceTry
                    [ do
                        reserved "find"
                        is <- parse `sepBy1` comma
                        colon
                        j <- parse
                        return [ Find i j | i <- is ]
                        <?> "find statement"
                    , do
                        reserved "given"
                        is <- parse `sepBy1` comma
                        colon
                        j <- parse
                        return [ Given i j | i <- is ]
                        <?> "given statement"
                    , do
                        reserved "letting"
                        is <- parse `sepBy1` comma
                        reserved "be"
                        reserved "new"
                        reserved "type"
                        j <- parse
                        case j of
                            TEnum {}    -> return ()
                            TUnnamed {} -> return ()
                            _           -> fail ""
                        return [ LettingType i j | i <- is ]
                        <?> "letting statement"
                    , do
                        reserved "given"
                        is <- parse `sepBy1` comma
                        colon
                        reserved "new"
                        reserved "type"
                        reserved "enum"
                        return [ GivenType i (TEnum Nothing) | i <- is ]
                        <?> "given statement"
                    , do
                        reserved "letting"
                        is <- parse `sepBy1` comma
                        reserved "be"
                        reserved "domain"
                        j <- parse
                        return [ LettingDomain i j | i <- is ]
                        <?> "letting statement"
                    , do
                        reserved "letting"
                        is <- parse `sepBy1` comma
                        reserved "be"
                        j <- parse
                        return [ LettingExpr i j | i <- is ]
                        <?> "letting statement"
                    , do
                        reserved "letting"
                        is <- parse `sepBy1` comma
                        reserved "be"
                        reserved "lambda"
                        j <- parse
                        return [ LettingLambda i j | i <- is ]
                        <?> "letting statement"
                    , do
                        reserved "letting"
                        is <- parse `sepBy1` comma
                        reserved "be"
                        j <- parse
                        return [ LettingQuan i j | i <- is ]
                        <?> "letting statement"
                    ]
        concat <$> many1 one
    pretty = Pr.vcat . map pretty

instance ParsePrint [Either Binding Where] where
    parse = concatMap (\ t -> case t of Left  bs -> map Left  bs
                                        Right ws -> map Right ws
                      ) <$> many parse
    pretty = Pr.vcat . map pretty

