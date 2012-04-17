module Language.Essence.Expr where

import Data.Generics ( Data )
import Data.String ( IsString )
import Data.Typeable ( Typeable )
-- import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary )

import GenericOps.Core ( NodeTag, Hole, GPlate, MatchBind )
import ParsePrint ( ParsePrint )
import PrintUtils ( Doc )
import Language.EssenceLexerP ( Parser )
import {-# SOURCE #-} Language.Essence.Domain
import {-# SOURCE #-} Language.Essence.Type


isAtomicExpr :: Expr -> Bool

prettyExprTopLevel :: Expr -> Doc

pDimExpr :: Parser Expr

data Expr

instance Eq Expr
instance Ord Expr
instance Read Expr
instance Show Expr
instance Data Expr
instance Typeable Expr
-- instance Generic Expr

instance IsString Expr

instance NodeTag Expr
instance Hole Expr
instance GPlate Expr
instance MatchBind Expr
instance ParsePrint Expr
instance Arbitrary Expr
instance TypeOf Expr
instance DomainOf Expr
