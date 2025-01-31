{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Conjure.Representations.Internal
    ( Representation(..)
    , TypeOf_ReprCheck, TypeOf_DownD, TypeOf_SymmetryOrdering, TypeOf_Structural, TypeOf_DownC, TypeOf_Up
    , DomainX, DomainC
    , DispatchFunction, ReprOptionsFunction
    , rDownToX
    , mkOutName
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import qualified Data.Kind  as T (Type)

type DomainX x = Domain HasRepresentation x
type DomainC = Domain HasRepresentation Constant

-- | This data type represents a representation selection rule.
-- The 3 functions -- rDownD, rDownC, and rUp -- all work one level at a time.
-- The maybe for rDownD and rDownC is Nothing when representation doesn't change anything.
-- Like for primitives.
-- * rDownD is for refining a class level domain.
-- * rDownC is for refining constants, together with an instance level domain.
-- * rUp is for translating low level constants upwards
--   It takes in a instance level domain for the high level object.
-- * rCheck is for calculating all representation options.
--   It take a function to be used as a "checker" for inner domains, if any.
data Representation (m :: T.Type -> T.Type) = Representation
    { rCheck            :: TypeOf_ReprCheck        m
    , rDownD            :: TypeOf_DownD            m
    , rStructural       :: TypeOf_Structural       m
    , rDownC            :: TypeOf_DownC            m
    , rUp               :: TypeOf_Up               m
    , rSymmetryOrdering :: TypeOf_SymmetryOrdering m
    }

type TypeOf_ReprCheck (m :: T.Type -> T.Type) = 
       forall x . (Data x, Pretty x, ExpressionLike x)
    => (Domain () x -> m [DomainX x])               -- other checkers for inner domains
    -> Domain () x                                  -- this domain
    -> m [DomainX x]                                -- with all repr options

type TypeOf_DownD (m :: T.Type -> T.Type) = 
                (Name, DomainX Expression)
    -> m (Maybe [(Name, DomainX Expression)])

type TypeOf_SymmetryOrdering (m :: T.Type -> T.Type) = 
       ((Expression -> m [Expression]) -> Expression -> DomainX Expression -> m Expression) -- inner S.O.
    -> (Expression -> m [Expression])               -- general downX1
    -> Expression                                   -- this as an expression
    -> DomainX Expression                           -- name and domain
    -> m Expression                                 -- output, of type [int]

type TypeOf_Structural (m :: T.Type -> T.Type) = 
       (DomainX Expression -> m (Expression -> m [Expression]))
                                                    -- other structural constraints for inner domains
    -> (Expression -> m [Expression])               -- general downX1
    -> DomainX Expression                           -- this domain
    -> m (      Expression                          -- the original variable, before refinement
          -> m [Expression]                         -- structural constraints
         )

type TypeOf_DownC (m :: T.Type -> T.Type) = 
                 (Name, DomainC, Constant)          -- the input name, domain and constant
    -> m (Maybe [(Name, DomainC, Constant)])        -- the outputs names, domains, and constants

type TypeOf_Up (m :: T.Type -> T.Type) = 
        [(Name, Constant)] ->                       -- all known constants, representing a solution at the low level
        (Name, DomainC) ->                          -- the name and domain we are working on
        m (Name, Constant)                          -- the output constant, at the high level


type DispatchFunction m x =
        Data x =>
        Pretty x =>
        Domain HasRepresentation x ->
        Representation m

type ReprOptionsFunction m r x =
        Data x =>
        Pretty x =>
        ExpressionLike x =>
        Domain () x ->
        m [Domain HasRepresentation x]


rDownToX ::
    (Monad m,MonadFail m) =>
    Representation m ->                             -- for a given representation
    FindOrGiven ->                                  -- and a declaration: forg
    Name ->                                         --                  : name
    Domain HasRepresentation Expression ->          --                  : domain
    m [Expression]                                  -- expressions referring to the representation
rDownToX repr forg name domain = do
    pairs <- rDownD repr (name, domain)
    return [ Reference n (Just (DeclHasRepr forg n d))
           | (n,d) <- concat pairs
           ]

mkOutName :: Maybe Name -> Domain HasRepresentation x -> Name -> Name
mkOutName Nothing       domain origName = mconcat [origName, "_", Name (reprTreeEncoded domain)]
mkOutName (Just suffix) domain origName = mconcat [origName, "_", Name (reprTreeEncoded domain), "_", suffix]
