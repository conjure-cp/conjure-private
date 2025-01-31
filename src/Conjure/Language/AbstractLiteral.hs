{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.AbstractLiteral where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError ( failToUserError )
import Conjure.Language.Name
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.AdHoc

import Conjure.Language.TypeOf
import Conjure.Language.Pretty

-- aeson
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V               -- vector


data AbstractLiteral x
    = AbsLitTuple [x]
    | AbsLitRecord [(Name, x)]
    | AbsLitVariant (Maybe [(Name, Domain () x)]) Name x            -- Nothing before name resolution
    | AbsLitMatrix (Domain () x) [x]                                -- the domain is the index domain
    | AbsLitSet [x]
    | AbsLitMSet [x]
    | AbsLitFunction [(x, x)]
    | AbsLitSequence [x]
    | AbsLitRelation [[x]]
    | AbsLitPartition [[x]]
    | AbsLitPermutation [[x]]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (AbstractLiteral x)
instance Hashable  x => Hashable  (AbstractLiteral x)
instance ToJSON    x => ToJSON    (AbstractLiteral x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (AbstractLiteral x) where parseJSON = genericParseJSON jsonOptions

instance (SimpleJSON x, Pretty x, ExpressionLike x) => SimpleJSON (AbstractLiteral x) where
    toSimpleJSON lit =
        case lit of
            AbsLitTuple xs -> toSimpleJSON xs
            AbsLitRecord xs -> do
                xs' <- forM xs $ \ (nm, x) -> do
                    x' <- toSimpleJSON x
                    return (fromString (renderNormal nm), x')
                return $ JSON.Object $ KM.fromList xs'
            AbsLitVariant _ nm x -> do
                x' <- toSimpleJSON x
                return $ JSON.Object $ KM.fromList [(fromString (renderNormal nm), x')]
            AbsLitSequence xs -> toSimpleJSON xs
            AbsLitMatrix index xs ->
                case index of
                    DomainInt _ ranges -> do
                        indices <- failToUserError $ rangesInts ranges
                        toSimpleJSON (AsDictionary (zip indices xs))
                    _ -> toSimpleJSON xs
            AbsLitSet xs -> toSimpleJSON xs
            AbsLitMSet xs -> toSimpleJSON xs
            AbsLitFunction xs -> toSimpleJSON (AsDictionary xs)
            AbsLitRelation xs -> toSimpleJSON xs
            AbsLitPartition xs -> toSimpleJSON xs
            AbsLitPermutation xs -> toSimpleJSON xs
    fromSimpleJSON = noFromSimpleJSON "AbstractLiteral"

instance (ToFromMiniZinc x, Pretty x, ExpressionLike x) => ToFromMiniZinc (AbstractLiteral x) where
    toMiniZinc lit =
        case lit of
            AbsLitTuple xs -> MZNArray Nothing <$> mapM toMiniZinc xs
            AbsLitMatrix (DomainInt _ [RangeSingle r]) xs -> MZNArray (Just $ show $ pretty r <> ".." <> pretty r) <$> mapM toMiniZinc xs
            AbsLitMatrix (DomainInt _ [r]) xs -> MZNArray (Just $ show $ pretty r) <$> mapM toMiniZinc xs
            AbsLitMatrix _index xs -> MZNArray Nothing <$> mapM toMiniZinc xs
            AbsLitSet xs ->
                case xs of
                    (x:_) | Just _ <- intOut "toMiniZinc" x -> MZNSet <$> mapM toMiniZinc xs
                    _ -> MZNArray Nothing <$> mapM toMiniZinc xs
            AbsLitMSet xs -> MZNArray Nothing <$> mapM toMiniZinc xs
            AbsLitFunction xs -> MZNArray Nothing <$> mapM (toMiniZinc . snd) xs
            AbsLitSequence xs -> MZNArray Nothing <$> mapM toMiniZinc xs
            AbsLitRelation xss ->
                MZNArray Nothing <$> forM xss (\ xs ->
                    MZNArray Nothing <$> mapM toMiniZinc xs)
            AbsLitPartition xss ->
                MZNArray Nothing <$> forM xss (\ xs ->
                    MZNArray Nothing <$> mapM toMiniZinc xs)
            _ -> noToMiniZinc lit

instance Pretty a => Pretty (AbstractLiteral a) where
    pretty (AbsLitTuple xs) = (if length xs < 2 then "tuple" else prEmpty) <+> prettyList prParens "," xs
    pretty (AbsLitRecord xs) = "record" <+> prettyList prBraces "," [ pretty n <+> "=" <++> pretty x
                                                                    | (n,x) <- xs ]
    pretty (AbsLitVariant _ n x) = "variant" <+> prBraces (pretty n <+> "=" <+> pretty x)
    pretty (AbsLitMatrix _     []) = "[]"
    pretty (AbsLitMatrix index xs) = let f i = prBrackets (i <> ";" <++> pretty index) in prettyList f "," xs
    pretty (AbsLitSet       xs ) =                prettyList prBraces "," xs
    pretty (AbsLitMSet      xs ) = "mset"      <> prettyList prParens "," xs
    pretty (AbsLitFunction  xs ) = "function"  <> prettyListDoc prParens "," [ pretty a <++> "-->" <+> pretty b | (a,b) <- xs ]
    pretty (AbsLitSequence  xs ) = "sequence"  <> prettyList prParens "," xs
    pretty (AbsLitRelation  xss) = "relation"  <> prettyListDoc prParens "," [ pretty (AbsLitTuple xs)         | xs <- xss   ]
    pretty (AbsLitPartition xss) = "partition" <> prettyListDoc prParens "," [ prettyList prBraces "," xs      | xs <- xss   ]
    pretty (AbsLitPermutation xss) = "permutation" <> prettyListDoc prParens "," [ prettyList prParens "," xs | xs <- xss ]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (AbstractLiteral x) where
    varSymBreakingDescription (AbsLitTuple xs) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "AbsLitTuple")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        ]
    varSymBreakingDescription AbsLitRecord{} = JSON.Object $ KM.fromList
        [ ("type", JSON.String "AbsLitRecord")
        ]
    varSymBreakingDescription AbsLitVariant{} = JSON.Object $ KM.fromList
        [ ("type", JSON.String "AbsLitVariant")
        ]
    varSymBreakingDescription (AbsLitMatrix _ xs) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "AbsLitMatrix")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        ]
    varSymBreakingDescription (AbsLitSet xs) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "AbsLitSet")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (AbsLitMSet xs) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "AbsLitMSet")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (AbsLitFunction xs) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "AbsLitFunction")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription (AbsLitTuple [x,y]) | (x,y) <- xs ])
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (AbsLitSequence xs) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "AbsLitSequence")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription (AbsLitTuple [fromInt i, x]) | (i,x) <- zip allNats xs ])
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (AbsLitRelation xs) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "AbsLitRelation")
        , ("children", JSON.Array $ V.fromList $ map (varSymBreakingDescription . AbsLitTuple) xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (AbsLitPartition xs) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "AbsLitPartition")
        , ("children", JSON.Array $ V.fromList $ map (varSymBreakingDescription . AbsLitSet) xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (AbsLitPermutation xs) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "AbsLitPermutation")
        , ("children", JSON.Array $ V.fromList $ map (varSymBreakingDescription . AbsLitSequence) xs)
        , ("symmetricChildren", JSON.Bool True)
        ]

instance (TypeOf a, Pretty a) => TypeOf (AbstractLiteral a) where

    typeOf   (AbsLitTuple        []) = return (TypeTuple [TypeAny])
    typeOf   (AbsLitTuple        xs) = TypeTuple    <$> mapM typeOf xs

    typeOf   (AbsLitRecord       xs) = TypeRecord   <$> sequence [ do t <- typeOf x ; return (n,t)
                                                                 | (n,x) <- xs ]

    typeOf   (AbsLitVariant Nothing  _ _) = failDoc "Cannot calculate the type of variant literal."
    typeOf   (AbsLitVariant (Just t) _ _) = fmap TypeVariant $ forM t $ \ (n,d) -> do
        dt <- typeOfDomain d
        return (n, dt)

    typeOf   (AbsLitMatrix _   []  ) = return (TypeMatrix TypeAny TypeAny)
    typeOf p@(AbsLitMatrix ind inn ) = TypeMatrix   <$> typeOfDomain ind <*> (homoType (pretty p) =<< mapM typeOf inn)

    typeOf   (AbsLitSet         [] ) = return (TypeSet TypeAny)
    typeOf p@(AbsLitSet         xs ) = TypeSet      <$> (homoType (pretty p) =<< mapM typeOf xs)

    typeOf   (AbsLitMSet        [] ) = return (TypeMSet TypeAny)
    typeOf p@(AbsLitMSet        xs ) = TypeMSet     <$> (homoType (pretty p) =<< mapM typeOf xs)

    typeOf   (AbsLitFunction    [] ) = return (TypeFunction TypeAny TypeAny)
    typeOf p@(AbsLitFunction    xs ) = TypeFunction <$> (homoType (pretty p) =<< mapM (typeOf . fst) xs)
                                                    <*> (homoType (pretty p) =<< mapM (typeOf . snd) xs)

    typeOf   (AbsLitSequence    [] ) = return (TypeSequence TypeAny)
    typeOf p@(AbsLitSequence    xs ) = TypeSequence <$> (homoType (pretty p) =<< mapM typeOf xs)

    typeOf   (AbsLitRelation    [] ) = return (TypeRelation [TypeAny])
    typeOf p@(AbsLitRelation    xss) = do
        ty <- homoType (pretty p) =<< mapM (typeOf . AbsLitTuple) xss
        case ty of
            TypeTuple ts -> return (TypeRelation ts)
            _ -> bug "expecting TypeTuple in typeOf"

    typeOf   (AbsLitPartition   [] ) = return (TypePartition TypeAny)
    typeOf p@(AbsLitPartition   xss) = TypePartition <$> (homoType (pretty p) =<< mapM typeOf (concat xss))
    typeOf   (AbsLitPermutation [] ) = return (TypePermutation TypeAny)
    typeOf p@(AbsLitPermutation xss) = TypePermutation <$> (homoType (pretty p) =<< mapM typeOf (concat xss))


normaliseAbsLit :: (Ord c, ExpressionLike c) => (c -> c) -> AbstractLiteral c -> AbstractLiteral c
normaliseAbsLit norm (AbsLitTuple     xs ) = AbsLitTuple                           $ map norm xs
normaliseAbsLit norm (AbsLitRecord    xs ) = AbsLitRecord                          $ map (second norm) xs
normaliseAbsLit norm (AbsLitVariant t n x) = AbsLitVariant t n (norm x)
normaliseAbsLit norm (AbsLitMatrix d  xs ) = AbsLitMatrix (normaliseDomain norm d) $ map norm xs
normaliseAbsLit norm (AbsLitSet       xs ) = AbsLitSet                   $ sortNub $ map norm xs
normaliseAbsLit norm (AbsLitMSet      xs ) = AbsLitMSet                  $ sort    $ map norm xs
normaliseAbsLit norm (AbsLitFunction  xs ) = AbsLitFunction              $ sortNub [ (norm x, norm y) | (x, y) <- xs ]
normaliseAbsLit norm (AbsLitSequence  xs ) = AbsLitSequence              $           map norm xs
normaliseAbsLit norm (AbsLitRelation  xss) = AbsLitRelation              $ sortNub $ map (map norm) xss
normaliseAbsLit norm (AbsLitPartition xss) = AbsLitPartition             $ sortNub $ map (sortNub . map norm) xss
normaliseAbsLit norm (AbsLitPermutation xss) = AbsLitPermutation $ map (map norm) xss 

emptyCollectionAbsLit :: AbstractLiteral c -> Bool
emptyCollectionAbsLit AbsLitTuple{} = False
emptyCollectionAbsLit AbsLitRecord{} = False
emptyCollectionAbsLit AbsLitVariant{} = False
emptyCollectionAbsLit (AbsLitMatrix _ xs) = null xs
emptyCollectionAbsLit (AbsLitSet xs) = null xs
emptyCollectionAbsLit (AbsLitMSet xs) = null xs
emptyCollectionAbsLit (AbsLitFunction xs) = null xs
emptyCollectionAbsLit (AbsLitSequence xs) = null xs
emptyCollectionAbsLit (AbsLitRelation xs) = null xs
emptyCollectionAbsLit (AbsLitPartition xs) = null xs
emptyCollectionAbsLit (AbsLitPermutation xs) = null xs
