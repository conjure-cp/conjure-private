{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Conjure.Language.Domain
    ( Domain(..)
    , HasRepresentation(..)
    , Range(..), rangesInts
    , SetAttr(..), SizeAttr(..), getMaxFrom_SizeAttr, intersectSizeAttr
    , MSetAttr(..), OccurAttr(..), getMaxFrom_OccurAttr
    , FunctionAttr(..), PartialityAttr(..), JectivityAttr(..)
    , SequenceAttr(..)
    , RelationAttr(..), BinaryRelationAttrs(..), BinaryRelationAttr(..), binRelNames
    , PartitionAttr(..)
    , PermutationAttr(..)
    , AttrName(..)
    , DomainAttributes(..), DomainAttribute(..)         -- only for parsing
    , textToRepresentation, representationToShortText, representationToFullText
    , isPrimitiveDomain, domainCanIndexMatrix, expandDomainReference, getIndices
    , Tree(..), reprTree, reprAtTopLevel, applyReprTree
    , reprTreeEncoded
    , forgetRepr, changeRepr, defRepr
    , mkDomainBool, mkDomainInt, mkDomainIntB, mkDomainIntBTagged, mkDomainAny
    , typeOfDomain
    , readBinRel, binRelToAttrName
    , normaliseDomain, normaliseRange
    , innerDomainOf
    , singletonDomainInt
    , matrixNumDimsD
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.AdHoc
import Conjure.Language.Pretty

-- base
import qualified Data.Semigroup as Semigroup ( (<>) )

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), choose, oneof, vectorOf, sized )

-- containers
import Data.Set as S ( Set, empty, toList, union )

-- syb
import Data.Data ( toConstr, constrIndex )


data Domain r x
    = DomainAny Text Type
    | DomainBool
    | DomainIntE IntTag x
    | DomainInt IntTag [Range x]
    | DomainEnum
        Name
        (Maybe [Range x])           -- subset of values for this domain
                                    -- Nothing *only* when GivenDomainDefnEnum and not LettingDomainDefnEnum
        (Maybe [(Name, Integer)])   -- the mapping to integers, if available
    | DomainUnnamed Name x
    | DomainTuple [Domain r x]
    | DomainRecord [(Name, Domain r x)]
    | DomainVariant [(Name, Domain r x)]
    | DomainMatrix (Domain () x) (Domain r x)
    | DomainSet       r (SetAttr x) (Domain r x)
    | DomainMSet      r (MSetAttr x) (Domain r x)
    | DomainFunction  r (FunctionAttr x) (Domain r x) (Domain r x)
    | DomainSequence  r (SequenceAttr x) (Domain r x)
    | DomainRelation  r (RelationAttr x) [Domain r x]
    | DomainPartition r (PartitionAttr x) (Domain r x)
    | DomainPermutation r (PermutationAttr x) (Domain r x)
    | DomainOp Name [Domain r x]
    | DomainReference Name (Maybe (Domain r x))
    | DomainMetaVar String
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance (VarSymBreakingDescription x, ToJSON r) => VarSymBreakingDescription (Domain r x) where
    varSymBreakingDescription domain = toJSON $ fmap varSymBreakingDescription domain

mkDomainBool :: Domain () x
mkDomainBool = DomainBool

mkDomainInt :: [Range x] -> Domain () x
mkDomainInt = DomainInt TagInt

mkDomainIntB :: x -> x -> Domain () x
mkDomainIntB l u = DomainInt TagInt [RangeBounded l u]

mkDomainIntBTagged :: IntTag -> x -> x -> Domain () x
mkDomainIntBTagged t l u = DomainInt t [RangeBounded l u]

mkDomainAny :: Doc -> Type -> Domain r x
mkDomainAny reason = DomainAny (stringToText $ show reason)

instance (Serialize r, Serialize x) => Serialize (Domain r x)
instance (Hashable  r, Hashable  x) => Hashable  (Domain r x)
instance (ToJSON    r, ToJSON    x) => ToJSON    (Domain r x) where toJSON = genericToJSON jsonOptions
instance (FromJSON  r, FromJSON  x) => FromJSON  (Domain r x) where parseJSON = genericParseJSON jsonOptions

instance Arbitrary x => Arbitrary (Domain r x) where
    arbitrary = sized f
        where
            f 0 = oneof [ return DomainBool
                        , DomainInt TagInt <$> arbitrary
                        -- , DomainEnum <$> arbitrary <*> arbitrary
                        ]
            f s = do
                arity <- choose (2 :: Int, 10)
                DomainTuple <$> vectorOf arity (f (div s 10))
    shrink DomainBool = []
    shrink (DomainInt _ []) = [DomainBool]
    shrink (DomainInt t [r]) = DomainBool : DomainInt t [] : [DomainInt t [r'] | r' <- shrink r]
    shrink (DomainInt t rs) = [DomainInt t (init rs)]
    shrink _ = []


typeOfDomain ::
    MonadFailDoc m =>
    Pretty r =>
    TypeOf x =>
    Pretty x =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Domain r x -> m Type
typeOfDomain (DomainAny _ ty)          = return ty
typeOfDomain DomainBool                = return TypeBool
typeOfDomain d@(DomainIntE t x)          = do
    ty <- typeOf x
    case ty of
        TypeInt TagInt                -> return ()       -- pre recoverDomainInt
        TypeList     (TypeInt TagInt) -> return ()
        TypeMatrix _ (TypeInt TagInt) -> return ()
        TypeSet      (TypeInt TagInt) -> return ()
        _ -> failDoc $ vcat [ "Expected an integer, but got:" <++> pretty ty
                            , "In domain:" <+> pretty d
                            ]
    return (TypeInt t)
typeOfDomain d@(DomainInt t rs)        = do
    forM_ rs $ \ r -> forM_ r $ \ x -> do
        ty <- typeOf x
        case ty of
            TypeInt _ -> return ()
            _ -> failDoc $ vcat [ "Expected an integer, but got:" <++> pretty ty
                                , "For:" <+> pretty x
                                , "In domain:" <+> pretty d
                                ]
    return (TypeInt t)
typeOfDomain (DomainEnum    defn _ _ ) = return (TypeEnum defn)
typeOfDomain (DomainUnnamed defn _   ) = return (TypeUnnamed defn)
typeOfDomain (DomainTuple         xs ) = TypeTuple      <$> mapM typeOfDomain xs
typeOfDomain (DomainRecord        xs ) = TypeRecord     <$> sequence [ do t <- typeOfDomain d ; return (n, t)
                                                                     | (n,d) <- xs ]
typeOfDomain (DomainVariant       xs ) = TypeVariant    <$> sequence [ do t <- typeOfDomain d ; return (n, t)
                                                                     | (n,d) <- xs ]
typeOfDomain (DomainMatrix ind inn   ) = TypeMatrix     <$> typeOfDomain ind <*> typeOfDomain inn
typeOfDomain (DomainSet       _ _ x  ) = TypeSet        <$> typeOfDomain x
typeOfDomain (DomainMSet      _ _ x  ) = TypeMSet       <$> typeOfDomain x
typeOfDomain (DomainFunction  _ _ x y) = TypeFunction   <$> typeOfDomain x <*> typeOfDomain y
typeOfDomain (DomainSequence  _ _ x  ) = TypeSequence   <$> typeOfDomain x
typeOfDomain (DomainRelation  _ _ xs ) = TypeRelation   <$> mapM typeOfDomain xs
typeOfDomain (DomainPartition _ _ x  ) = TypePartition  <$> typeOfDomain x
typeOfDomain (DomainPermutation _ _ x )  = TypePermutation <$> typeOfDomain x
typeOfDomain p@(DomainOp _ ds) = do
    ts <- mapM typeOfDomain ds
    if typesUnify ts
        then return (mostDefined ts)
        else failDoc ("Type error in" <+> pretty p)
typeOfDomain (DomainReference _ (Just d)) = typeOfDomain d
typeOfDomain (DomainReference nm Nothing) = bug $ "typeOfDomain: DomainReference" <+> pretty nm
typeOfDomain (DomainMetaVar nm) = bug $ "typeOfDomain: DomainMetaVar &" <> pretty nm

forgetRepr :: Domain r x -> Domain () x
forgetRepr = defRepr

defRepr :: Default r2 => Domain r x -> Domain r2 x
defRepr = changeRepr def

changeRepr :: r2 -> Domain r x -> Domain r2 x
changeRepr rep = go
    where
        go (DomainAny t ty) = DomainAny t ty
        go DomainBool = DomainBool
        go (DomainIntE t x) = DomainIntE t x
        go (DomainInt t rs) = DomainInt t rs
        go (DomainEnum defn rs mp) = DomainEnum defn rs mp
        go (DomainUnnamed defn s) = DomainUnnamed defn s
        go (DomainTuple ds) = DomainTuple (map go ds)
        go (DomainRecord xs) = DomainRecord (map (second go) xs)
        go (DomainVariant xs) = DomainVariant (map (second go) xs)
        go (DomainMatrix index inner) = DomainMatrix index (go inner)
        go (DomainSet _   attr d) =
            DomainSet rep attr (go d)
        go (DomainMSet _   attr d) =
            DomainMSet rep attr (go d)
        go (DomainFunction _   attr d1 d2) =
            DomainFunction rep attr (go d1) (go d2)
        go (DomainSequence _   attr d) =
            DomainSequence rep attr (go d)
        go (DomainRelation _   attr ds) =
            DomainRelation rep attr (map go ds)
        go (DomainPartition _   attr d) = DomainPartition rep attr (go d)
        go (DomainPermutation _ attr d) = DomainPermutation rep attr (go d)
        go (DomainOp op ds) = DomainOp op (map go ds)
        go (DomainReference x r) = DomainReference x (fmap go r)
        go (DomainMetaVar x) = DomainMetaVar x


data Tree a = Tree { rootLabel :: a, subForest :: [Tree a] }
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize a => Serialize (Tree a)
instance Hashable  a => Hashable  (Tree a)
instance ToJSON    a => ToJSON    (Tree a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (Tree a) where parseJSON = genericParseJSON jsonOptions

-- | This is to be used when defining `Conjure.Representations.Internal.mkOutName`.
--   Reason is to avoid sharing variables for parts of the same decision variable with differing representations.
--   Example case:
--      (1) find x : set {A} of (int(a..b) , set {B} of int(c..d))
--      (2) find x : set {A} of (int(a..b) , set {C} of int(c..d))
--      Here x_1's should not be shared!
--      If they are, the channelling and symmetry breaking constraints will clash and solutions will be lost.
reprTreeEncoded :: Domain HasRepresentation x -> Text
reprTreeEncoded = mconcat . enc1 . reprTree
    where
        enc1 (Tree lbl sub) =
            maybe
                (bug "reprTreeEncoded: top-most representation is Nothing")
                representationToShortText
                lbl
            : concatMap enc sub
        enc (Tree lbl sub) =
            maybe [] representationConstrIndex lbl
            ++ concatMap enc sub

reprTree :: Domain r x -> Tree (Maybe r)
reprTree DomainAny{}     = Tree Nothing []
reprTree DomainBool{}    = Tree Nothing []
reprTree DomainIntE{}    = Tree Nothing []
reprTree DomainInt{}     = Tree Nothing []
reprTree DomainEnum{}    = Tree Nothing []
reprTree DomainUnnamed{} = Tree Nothing []
reprTree (DomainTuple  as ) = Tree Nothing (map reprTree as)
reprTree (DomainRecord as ) = Tree Nothing (map (reprTree . snd) as)
reprTree (DomainVariant as) = Tree Nothing (map (reprTree . snd) as)
reprTree (DomainMatrix _ a) = Tree Nothing [reprTree a]
reprTree (DomainSet       r _ a  ) = Tree (Just r) [reprTree a]
reprTree (DomainMSet      r _ a  ) = Tree (Just r) [reprTree a]
reprTree (DomainFunction  r _ a b) = Tree (Just r) [reprTree a, reprTree b]
reprTree (DomainSequence  r _ a  ) = Tree (Just r) [reprTree a]
reprTree (DomainRelation  r _ as ) = Tree (Just r) (map reprTree as)
reprTree (DomainPartition r _ a  ) = Tree (Just r) [reprTree a]
reprTree (DomainPermutation r _ a)   = Tree (Just r) [reprTree a]
reprTree DomainOp{}        = Tree Nothing []
reprTree DomainReference{} = Tree Nothing []
reprTree DomainMetaVar{}   = Tree Nothing []

reprAtTopLevel :: Domain r x -> Maybe r
reprAtTopLevel = rootLabel . reprTree

applyReprTree :: (MonadFailDoc m, Pretty x, Pretty r2, Default r) => Domain r2 x -> Tree (Maybe r) -> m (Domain r x)
applyReprTree dom@DomainBool{}    (Tree Nothing []) = return (defRepr dom)
applyReprTree dom@DomainInt{}     (Tree Nothing []) = return (defRepr dom)
applyReprTree dom@DomainIntE{}    (Tree Nothing []) = return (defRepr dom)
applyReprTree dom@DomainEnum{}    (Tree Nothing []) = return (defRepr dom)
applyReprTree dom@DomainUnnamed{} (Tree Nothing []) = return (defRepr dom)
applyReprTree (DomainTuple as  ) (Tree Nothing asRepr) =
    DomainTuple <$> zipWithM applyReprTree as asRepr
applyReprTree (DomainRecord as ) (Tree Nothing asRepr) =
    DomainRecord  . zip (map fst as) <$> zipWithM applyReprTree (map snd as) asRepr
applyReprTree (DomainVariant as) (Tree Nothing asRepr) =
    DomainVariant . zip (map fst as) <$> zipWithM applyReprTree (map snd as) asRepr
applyReprTree (DomainMatrix b a) (Tree Nothing [aRepr]) = DomainMatrix b <$> applyReprTree a aRepr
applyReprTree (DomainSet       _ attr a  ) (Tree (Just r) [aRepr]) = DomainSet r attr <$> applyReprTree a aRepr
applyReprTree (DomainMSet      _ attr a  ) (Tree (Just r) [aRepr]) = DomainMSet r attr <$> applyReprTree a aRepr
applyReprTree (DomainFunction  _ attr a b) (Tree (Just r) [aRepr, bRepr]) = DomainFunction r attr <$> applyReprTree a aRepr <*> applyReprTree b bRepr
applyReprTree (DomainSequence  _ attr a  ) (Tree (Just r) [aRepr]) = DomainSequence r attr <$> applyReprTree a aRepr
applyReprTree (DomainRelation  _ attr as ) (Tree (Just r) asRepr) = DomainRelation r attr <$> zipWithM applyReprTree as asRepr
applyReprTree (DomainPartition _ attr a  ) (Tree (Just r) [aRepr]) = DomainPartition r attr <$> applyReprTree a aRepr
applyReprTree (DomainPermutation _ attr a ) (Tree (Just r) [aRepr]) = DomainPermutation r attr <$> applyReprTree a aRepr
applyReprTree dom@DomainOp{}        (Tree Nothing []) = return (defRepr dom)
applyReprTree dom@DomainReference{} (Tree Nothing []) = return (defRepr dom)
applyReprTree dom@DomainMetaVar{}   (Tree Nothing []) = return (defRepr dom)
applyReprTree dom _ = failDoc $ "applyReprTree:" <++> pretty dom

isPrimitiveDomain :: Domain r x -> Bool
isPrimitiveDomain DomainBool{} = True
isPrimitiveDomain DomainIntE{} = True
isPrimitiveDomain DomainInt{} = True
isPrimitiveDomain (DomainMatrix index inner) = and [isPrimitiveDomain index, isPrimitiveDomain inner]
isPrimitiveDomain _ = False

getIndices :: Domain r x -> ([Domain () x], Domain r x)
getIndices (DomainMatrix index inner) = first (index:) (getIndices inner)
getIndices d = ([], d)

domainCanIndexMatrix :: Domain r x -> Bool
domainCanIndexMatrix (DomainReference _ (Just d)) = domainCanIndexMatrix d
domainCanIndexMatrix DomainBool{} = True
domainCanIndexMatrix DomainInt {} = True
domainCanIndexMatrix DomainIntE{} = True
domainCanIndexMatrix DomainEnum{} = True
domainCanIndexMatrix _            = False

expandDomainReference :: Data r => Data x => Domain r x -> Domain r x
expandDomainReference (DomainReference _ (Just d)) = expandDomainReference d
expandDomainReference d = descend expandDomainReference d


--------------------------------------------------------------------------------
-- attribute-as-constraint handling --------------------------------------------
--------------------------------------------------------------------------------

data AttrName
    = AttrName_size
    | AttrName_minSize
    | AttrName_maxSize
    | AttrName_minOccur
    | AttrName_maxOccur
    | AttrName_numParts
    | AttrName_minNumParts
    | AttrName_maxNumParts
    | AttrName_partSize
    | AttrName_minPartSize
    | AttrName_maxPartSize
    | AttrName_total
    | AttrName_injective
    | AttrName_surjective
    | AttrName_bijective
    | AttrName_regular
    | AttrName_numMoved
    | AttrName_minNumMoved
    | AttrName_maxNumMoved
    -- bin rel ones
    | AttrName_reflexive
    | AttrName_irreflexive
    | AttrName_coreflexive
    | AttrName_symmetric
    | AttrName_antiSymmetric
    | AttrName_aSymmetric
    | AttrName_transitive
    | AttrName_leftTotal
    | AttrName_rightTotal
    | AttrName_connex
    | AttrName_Euclidean
    | AttrName_serial
    | AttrName_equivalence
    | AttrName_partialOrder
    | AttrName_linearOrder
    | AttrName_weakOrder
    | AttrName_preOrder
    | AttrName_strictPartialOrder
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize AttrName
instance Hashable  AttrName
instance ToJSON    AttrName where toJSON = genericToJSON jsonOptions
instance FromJSON  AttrName where parseJSON = genericParseJSON jsonOptions

instance Pretty AttrName where
    pretty AttrName_size = "size"
    pretty AttrName_minSize = "minSize"
    pretty AttrName_maxSize = "maxSize"
    pretty AttrName_minOccur = "minOccur"
    pretty AttrName_maxOccur = "maxOccur"
    pretty AttrName_numParts = "numParts"
    pretty AttrName_minNumParts = "minNumParts"
    pretty AttrName_maxNumParts = "maxNumParts"
    pretty AttrName_partSize = "partSize"
    pretty AttrName_minPartSize = "minPartSize"
    pretty AttrName_maxPartSize = "maxPartSize"
    pretty AttrName_total = "total"
    pretty AttrName_injective = "injective"
    pretty AttrName_surjective = "surjective"
    pretty AttrName_bijective = "bijective"
    pretty AttrName_regular = "regular"
    pretty AttrName_numMoved = "numMoved"
    pretty AttrName_minNumMoved = "minNumMoved"
    pretty AttrName_maxNumMoved = "maxNumMoved"
    pretty AttrName_reflexive = "reflexive"
    pretty AttrName_irreflexive = "irreflexive"
    pretty AttrName_coreflexive = "coreflexive"
    pretty AttrName_symmetric = "symmetric"
    pretty AttrName_antiSymmetric = "antiSymmetric"
    pretty AttrName_aSymmetric = "aSymmetric"
    pretty AttrName_transitive = "transitive"
    pretty AttrName_leftTotal = "leftTotal"
    pretty AttrName_rightTotal = "rightTotal"
    pretty AttrName_connex = "connex"
    pretty AttrName_Euclidean = "Euclidean"
    pretty AttrName_serial = "serial"
    pretty AttrName_equivalence = "equivalence"
    pretty AttrName_partialOrder = "partialOrder"
    pretty AttrName_linearOrder = "linearOrder"
    pretty AttrName_weakOrder = "weakOrder"
    pretty AttrName_preOrder = "preOrder"
    pretty AttrName_strictPartialOrder = "strictPartialOrder"

instance IsString AttrName where
    fromString "size" = AttrName_size
    fromString "minSize" = AttrName_minSize
    fromString "maxSize" = AttrName_maxSize
    fromString "minOccur" = AttrName_minOccur
    fromString "maxOccur" = AttrName_maxOccur
    fromString "numParts" = AttrName_numParts
    fromString "minNumParts" = AttrName_minNumParts
    fromString "maxNumParts" = AttrName_maxNumParts
    fromString "partSize" = AttrName_partSize
    fromString "minPartSize" = AttrName_minPartSize
    fromString "maxPartSize" = AttrName_maxPartSize
    fromString "total" = AttrName_total
    fromString "injective" = AttrName_injective
    fromString "surjective" = AttrName_surjective
    fromString "bijective" = AttrName_bijective
    fromString "regular" = AttrName_regular
    fromString "reflexive" = AttrName_reflexive
    fromString "irreflexive" = AttrName_irreflexive
    fromString "coreflexive" = AttrName_coreflexive
    fromString "symmetric" = AttrName_symmetric
    fromString "antiSymmetric" = AttrName_antiSymmetric
    fromString "aSymmetric" = AttrName_aSymmetric
    fromString "transitive" = AttrName_transitive
    fromString "connex" = AttrName_connex
    fromString "leftTotal" = AttrName_leftTotal
    fromString "rightTotal" = AttrName_rightTotal
    fromString "Euclidean" = AttrName_Euclidean
    fromString "serial" = AttrName_serial
    fromString "equivalence" = AttrName_equivalence
    fromString "partialOrder" = AttrName_partialOrder
    fromString "linearOrder" = AttrName_linearOrder
    fromString "weakOrder" = AttrName_weakOrder
    fromString "preOrder" = AttrName_preOrder
    fromString s = bug $ "fromString{AttrName}:" <+> pretty s

binRelNames :: [String]
binRelNames = [ "reflexive", "irreflexive", "coreflexive"
              , "symmetric", "antiSymmetric", "aSymmetric"
              , "transitive", "total", "leftTotal", "rightTotal", "connex", "Euclidean"
              , "serial", "equivalence", "weakOrder", "preOrder", "partialOrder", "strictPartialOrder", "linearOrder"
              ]


--------------------------------------------------------------------------------
-- attribute definitions -------------------------------------------------------
--------------------------------------------------------------------------------

data SetAttr a = SetAttr (SizeAttr a)
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (SetAttr a)
instance Hashable  a => Hashable  (SetAttr a)
instance ToJSON    a => ToJSON    (SetAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (SetAttr a) where parseJSON = genericParseJSON jsonOptions
instance Default (SetAttr a) where def = SetAttr def
instance Pretty a => Pretty (SetAttr a) where
    pretty (SetAttr SizeAttr_None) = prEmpty
    pretty (SetAttr a) = prParens (pretty a)


data SizeAttr a
    = SizeAttr_None
    | SizeAttr_Size a
    | SizeAttr_MinSize a
    | SizeAttr_MaxSize a
    | SizeAttr_MinMaxSize a a
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (SizeAttr a)
instance Hashable  a => Hashable  (SizeAttr a)
instance ToJSON    a => ToJSON    (SizeAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (SizeAttr a) where parseJSON = genericParseJSON jsonOptions
instance Default (SizeAttr a) where def = SizeAttr_None
instance Pretty a => Pretty (SizeAttr a) where
    pretty SizeAttr_None = prEmpty
    pretty (SizeAttr_Size       x  ) = "size"    <+> pretty x
    pretty (SizeAttr_MinSize    x  ) = "minSize" <+> pretty x
    pretty (SizeAttr_MaxSize    x  ) = "maxSize" <+> pretty x
    pretty (SizeAttr_MinMaxSize x y) = "minSize" <+> pretty x <> ", maxSize" <+> pretty y


getMaxFrom_SizeAttr :: MonadFailDoc m => SizeAttr a -> m a
getMaxFrom_SizeAttr (SizeAttr_Size n) = return n
getMaxFrom_SizeAttr (SizeAttr_MaxSize n) = return n
getMaxFrom_SizeAttr (SizeAttr_MinMaxSize _ n) = return n
getMaxFrom_SizeAttr _ = failDoc "getMaxFrom_SizeAttr"

intersectSizeAttr :: SizeAttr a -> SizeAttr a -> SizeAttr a
intersectSizeAttr SizeAttr_None s = s
intersectSizeAttr s@SizeAttr_Size{} _ = s
intersectSizeAttr _ s@SizeAttr_Size{} = s
intersectSizeAttr s _ = s

data MSetAttr a = MSetAttr (SizeAttr a) (OccurAttr a)
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (MSetAttr a)
instance Hashable  a => Hashable  (MSetAttr a)
instance ToJSON    a => ToJSON    (MSetAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (MSetAttr a) where parseJSON = genericParseJSON jsonOptions
instance Default (MSetAttr a) where def = MSetAttr def def
instance Pretty a => Pretty (MSetAttr a) where
    pretty (MSetAttr a b) =
        let inside = filter ((""/=) . show) [ pretty a
                                        , pretty b
                                        ]
        in  if null inside
                then prEmpty
                else prettyList prParens "," inside


data OccurAttr a
    = OccurAttr_None
    | OccurAttr_MinOccur a
    | OccurAttr_MaxOccur a
    | OccurAttr_MinMaxOccur a a
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (OccurAttr a)
instance Hashable  a => Hashable  (OccurAttr a)
instance ToJSON    a => ToJSON    (OccurAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (OccurAttr a) where parseJSON = genericParseJSON jsonOptions
instance Default (OccurAttr a) where def = OccurAttr_None
instance Pretty a => Pretty (OccurAttr a) where
    pretty OccurAttr_None = prEmpty
    pretty (OccurAttr_MinOccur    x  ) = "minOccur" <+> pretty x
    pretty (OccurAttr_MaxOccur    x  ) = "maxOccur" <+> pretty x
    pretty (OccurAttr_MinMaxOccur x y) = "minOccur" <+> pretty x <> ", maxOccur" <+> pretty y


getMaxFrom_OccurAttr :: MonadFailDoc m => OccurAttr a -> m a
getMaxFrom_OccurAttr (OccurAttr_MaxOccur n) = return n
getMaxFrom_OccurAttr (OccurAttr_MinMaxOccur _ n) = return n
getMaxFrom_OccurAttr _ = failDoc "getMaxFrom_OccurAttr"


data FunctionAttr x
    = FunctionAttr (SizeAttr x) PartialityAttr JectivityAttr
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (FunctionAttr a)
instance Hashable  a => Hashable  (FunctionAttr a)
instance ToJSON    a => ToJSON    (FunctionAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (FunctionAttr a) where parseJSON = genericParseJSON jsonOptions
instance Default (FunctionAttr a) where def = FunctionAttr def def def
instance Pretty a => Pretty (FunctionAttr a) where
    pretty (FunctionAttr a b c) =
        let inside = filter ((""/=) . show) [pretty a, pretty b, pretty c]
        in  if null inside
                then prEmpty
                else prettyList prParens "," inside


data PartialityAttr
    = PartialityAttr_Partial
    | PartialityAttr_Total
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize PartialityAttr
instance Hashable  PartialityAttr
instance ToJSON    PartialityAttr where toJSON = genericToJSON jsonOptions
instance FromJSON  PartialityAttr where parseJSON = genericParseJSON jsonOptions
instance Default   PartialityAttr where def = PartialityAttr_Partial
instance Pretty    PartialityAttr where
    pretty PartialityAttr_Partial = prEmpty -- partial is the default
    pretty PartialityAttr_Total = "total"


data JectivityAttr
    = JectivityAttr_None
    | JectivityAttr_Injective
    | JectivityAttr_Surjective
    | JectivityAttr_Bijective
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize JectivityAttr
instance Hashable  JectivityAttr
instance ToJSON    JectivityAttr where toJSON = genericToJSON jsonOptions
instance FromJSON  JectivityAttr where parseJSON = genericParseJSON jsonOptions
instance Default   JectivityAttr where def = JectivityAttr_None
instance Pretty    JectivityAttr where
    pretty JectivityAttr_None = prEmpty
    pretty JectivityAttr_Injective = "injective"
    pretty JectivityAttr_Surjective = "surjective"
    pretty JectivityAttr_Bijective = "bijective"


data SequenceAttr x
    = SequenceAttr (SizeAttr x) JectivityAttr
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (SequenceAttr a)
instance Hashable  a => Hashable  (SequenceAttr a)
instance ToJSON    a => ToJSON    (SequenceAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (SequenceAttr a) where parseJSON = genericParseJSON jsonOptions
instance Default (SequenceAttr a) where def = SequenceAttr def def
instance Pretty a => Pretty (SequenceAttr a) where
    pretty (SequenceAttr a b) =
        let inside = filter ((""/=) . show) [pretty a, pretty b]
        in  if null inside
                then prEmpty
                else prettyList prParens "," inside


data RelationAttr a = RelationAttr (SizeAttr a) BinaryRelationAttrs
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (RelationAttr a)
instance Hashable  a => Hashable  (RelationAttr a)
instance ToJSON    a => ToJSON    (RelationAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (RelationAttr a) where parseJSON = genericParseJSON jsonOptions
instance Default (RelationAttr a) where def = RelationAttr def def
instance Pretty a => Pretty (RelationAttr a) where
    pretty (RelationAttr a b) =
        let inside = filter ((""/=) . show) [pretty a, pretty b]
        in  if null inside
                then prEmpty
                else prettyList prParens "," inside


data BinaryRelationAttrs = BinaryRelationAttrs (S.Set BinaryRelationAttr)
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize BinaryRelationAttrs
instance Hashable  BinaryRelationAttrs where hashWithSalt salt (BinaryRelationAttrs a) = hashWithSalt salt (S.toList a)
instance ToJSON    BinaryRelationAttrs where toJSON = genericToJSON jsonOptions
instance FromJSON  BinaryRelationAttrs where parseJSON = genericParseJSON jsonOptions
instance Default   BinaryRelationAttrs where def = BinaryRelationAttrs S.empty
instance Pretty BinaryRelationAttrs where
    pretty (BinaryRelationAttrs attrs) = prettyList id "," (S.toList attrs)
instance Semigroup BinaryRelationAttrs where
    (<>) (BinaryRelationAttrs a) (BinaryRelationAttrs b) = BinaryRelationAttrs (S.union a b)
instance Monoid BinaryRelationAttrs where
    mempty = BinaryRelationAttrs def
    


data BinaryRelationAttr
    = BinRelAttr_Reflexive
    | BinRelAttr_Irreflexive
    | BinRelAttr_Coreflexive
    | BinRelAttr_Symmetric
    | BinRelAttr_AntiSymmetric
    | BinRelAttr_ASymmetric
    | BinRelAttr_Transitive
    | BinRelAttr_Total
    | BinRelAttr_LeftTotal
    | BinRelAttr_RightTotal
    | BinRelAttr_Connex
    | BinRelAttr_Euclidean
    | BinRelAttr_Serial
    | BinRelAttr_Equivalence
    | BinRelAttr_PartialOrder
    | BinRelAttr_LinearOrder
    | BinRelAttr_WeakOrder
    | BinRelAttr_PreOrder
    | BinRelAttr_StrictPartialOrder
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Bounded, Enum)

instance Serialize BinaryRelationAttr
instance Hashable  BinaryRelationAttr
instance ToJSON    BinaryRelationAttr where toJSON = genericToJSON jsonOptions
instance FromJSON  BinaryRelationAttr where parseJSON = genericParseJSON jsonOptions
instance Pretty BinaryRelationAttr where
    pretty BinRelAttr_Reflexive          = "reflexive"
    pretty BinRelAttr_Irreflexive        = "irreflexive"
    pretty BinRelAttr_Coreflexive        = "coreflexive"
    pretty BinRelAttr_Symmetric          = "symmetric"
    pretty BinRelAttr_AntiSymmetric      = "antiSymmetric"
    pretty BinRelAttr_ASymmetric         = "aSymmetric"
    pretty BinRelAttr_Transitive         = "transitive"
    pretty BinRelAttr_Total              = "total"
    pretty BinRelAttr_LeftTotal          = "leftTotal"
    pretty BinRelAttr_RightTotal         = "rightTotal"
    pretty BinRelAttr_Connex             = "connex"
    pretty BinRelAttr_Euclidean          = "Euclidean"
    pretty BinRelAttr_Serial             = "serial"
    pretty BinRelAttr_Equivalence        = "equivalence"
    pretty BinRelAttr_PartialOrder       = "partialOrder"
    pretty BinRelAttr_LinearOrder        = "linearOrder"
    pretty BinRelAttr_WeakOrder          = "weakOrder"
    pretty BinRelAttr_PreOrder           = "preOrder"
    pretty BinRelAttr_StrictPartialOrder = "strictPartialOrder"


readBinRel :: MonadFailDoc m => AttrName -> m BinaryRelationAttr
readBinRel AttrName_reflexive          = return BinRelAttr_Reflexive
readBinRel AttrName_irreflexive        = return BinRelAttr_Irreflexive
readBinRel AttrName_coreflexive        = return BinRelAttr_Coreflexive
readBinRel AttrName_symmetric          = return BinRelAttr_Symmetric
readBinRel AttrName_antiSymmetric      = return BinRelAttr_AntiSymmetric
readBinRel AttrName_aSymmetric         = return BinRelAttr_ASymmetric
readBinRel AttrName_transitive         = return BinRelAttr_Transitive
readBinRel AttrName_total              = return BinRelAttr_Total
readBinRel AttrName_leftTotal          = return BinRelAttr_LeftTotal
readBinRel AttrName_rightTotal         = return BinRelAttr_RightTotal
readBinRel AttrName_connex             = return BinRelAttr_Connex
readBinRel AttrName_Euclidean          = return BinRelAttr_Euclidean
readBinRel AttrName_serial             = return BinRelAttr_Serial
readBinRel AttrName_equivalence        = return BinRelAttr_Equivalence
readBinRel AttrName_partialOrder       = return BinRelAttr_PartialOrder
readBinRel AttrName_strictPartialOrder = return BinRelAttr_StrictPartialOrder
readBinRel AttrName_linearOrder        = return BinRelAttr_LinearOrder
readBinRel AttrName_weakOrder          = return BinRelAttr_WeakOrder
readBinRel AttrName_preOrder           = return BinRelAttr_PreOrder
readBinRel a = failDoc $ "Not a binary relation attribute:" <+> pretty a

binRelToAttrName :: BinaryRelationAttr -> AttrName
binRelToAttrName BinRelAttr_Reflexive          = AttrName_reflexive
binRelToAttrName BinRelAttr_Irreflexive        = AttrName_irreflexive
binRelToAttrName BinRelAttr_Coreflexive        = AttrName_coreflexive
binRelToAttrName BinRelAttr_Symmetric          = AttrName_symmetric
binRelToAttrName BinRelAttr_AntiSymmetric      = AttrName_antiSymmetric
binRelToAttrName BinRelAttr_ASymmetric         = AttrName_aSymmetric
binRelToAttrName BinRelAttr_Transitive         = AttrName_transitive
binRelToAttrName BinRelAttr_Total              = AttrName_total
binRelToAttrName BinRelAttr_LeftTotal          = AttrName_leftTotal
binRelToAttrName BinRelAttr_RightTotal         = AttrName_rightTotal
binRelToAttrName BinRelAttr_Connex             = AttrName_connex
binRelToAttrName BinRelAttr_Euclidean          = AttrName_Euclidean
binRelToAttrName BinRelAttr_Serial             = AttrName_serial
binRelToAttrName BinRelAttr_Equivalence        = AttrName_equivalence
binRelToAttrName BinRelAttr_PartialOrder       = AttrName_partialOrder
binRelToAttrName BinRelAttr_LinearOrder        = AttrName_linearOrder
binRelToAttrName BinRelAttr_WeakOrder          = AttrName_weakOrder
binRelToAttrName BinRelAttr_PreOrder           = AttrName_preOrder
binRelToAttrName BinRelAttr_StrictPartialOrder = AttrName_strictPartialOrder




data PartitionAttr a = PartitionAttr
    { partsNum          :: SizeAttr a
    , partsSize         :: SizeAttr a
    , isRegular         :: Bool
    }
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (PartitionAttr a)
instance Hashable  a => Hashable  (PartitionAttr a)
instance ToJSON    a => ToJSON    (PartitionAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (PartitionAttr a) where parseJSON = genericParseJSON jsonOptions
instance Default (PartitionAttr a) where def = PartitionAttr def def False
instance Pretty a => Pretty (PartitionAttr a) where
    pretty (PartitionAttr a b c) =
        let inside = filter ((""/=) . show) [ prettyNum a
                                        , prettySize b
                                        , prettyReg c
                                        ]

            prettyNum SizeAttr_None = prEmpty
            prettyNum (SizeAttr_Size       x  ) = "numParts"    <+> pretty x
            prettyNum (SizeAttr_MinSize    x  ) = "minNumParts" <+> pretty x
            prettyNum (SizeAttr_MaxSize    x  ) = "maxNumParts" <+> pretty x
            prettyNum (SizeAttr_MinMaxSize x y) = "minNumParts" <+> pretty x <> ", maxNumParts" <+> pretty y

            prettySize SizeAttr_None = prEmpty
            prettySize (SizeAttr_Size       x  ) = "partSize"    <+> pretty x
            prettySize (SizeAttr_MinSize    x  ) = "minPartSize" <+> pretty x
            prettySize (SizeAttr_MaxSize    x  ) = "maxPartSize" <+> pretty x
            prettySize (SizeAttr_MinMaxSize x y) = "minPartSize" <+> pretty x <> ", maxPartSize" <+> pretty y

            prettyReg False = prEmpty
            prettyReg True  = "regular"

        in  if null inside
                then prEmpty
                else prettyList prParens "," inside



newtype PermutationAttr a = PermutationAttr
    {
        numMoved :: SizeAttr a
    }
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (PermutationAttr a)
instance Hashable  a => Hashable  (PermutationAttr a)
instance ToJSON    a => ToJSON    (PermutationAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (PermutationAttr a) where parseJSON = genericParseJSON jsonOptions
instance Default a => Default (PermutationAttr a) where def = PermutationAttr def
instance Pretty a => Pretty (PermutationAttr a) where
    pretty (PermutationAttr a ) =
        let inside = filter (/=prEmpty) [pretty a]
        in  if null inside
                then prEmpty
                else prettyList prParens "," inside




data DomainAttributes a = DomainAttributes [DomainAttribute a]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize a => Serialize (DomainAttributes a)
instance Hashable  a => Hashable  (DomainAttributes a)
instance ToJSON    a => ToJSON    (DomainAttributes a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (DomainAttributes a) where parseJSON = genericParseJSON jsonOptions

instance Default (DomainAttributes a) where
    def = DomainAttributes []


data DomainAttribute a
    = DAName Name
    | DANameValue Name a
    | DADotDot
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize a => Serialize (DomainAttribute a)
instance Hashable  a => Hashable  (DomainAttribute a)
instance ToJSON    a => ToJSON    (DomainAttribute a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (DomainAttribute a) where parseJSON = genericParseJSON jsonOptions


data Range a
    = RangeOpen
    | RangeSingle a
    | RangeLowerBounded a
    | RangeUpperBounded a
    | RangeBounded a a
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize a => Serialize (Range a)
instance Hashable  a => Hashable (Range a)
instance ToJSON    a => ToJSON (Range a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON (Range a) where parseJSON = genericParseJSON jsonOptions

instance Arbitrary a => Arbitrary (Range a) where
    arbitrary = oneof
        [ return RangeOpen
        , RangeSingle <$> arbitrary
        , RangeLowerBounded <$> arbitrary
        , RangeUpperBounded <$> arbitrary
        , RangeBounded <$> arbitrary <*> arbitrary
        ]

rangesInts :: (MonadFailDoc m, ExpressionLike c) => [Range c] -> m [Integer]
rangesInts = fmap (sortNub . concat) . mapM rangeInts
    where
        rangeInts (RangeSingle x) = return <$> intOut "rangeInts 1" x
        rangeInts (RangeBounded x y) = do x' <- intOut "rangeInts 2" x
                                          y' <- intOut "rangeInts 3" y
                                          return [x' .. y']
        rangeInts _ = failDoc "Infinite range (or not an integer range)"

expandRanges :: ExpressionLike c => [Range c] -> [Range c]
expandRanges [RangeBounded a b] = [RangeBounded a b]
expandRanges r =
    case rangesInts r of
        Nothing -> r
        Just [] -> []
        Just is ->
            if [ minimum is .. maximum is ] == is
                then [RangeBounded (fromInt (minimum is)) (fromInt (maximum is))]
                else map (RangeSingle . fromInt) is


data HasRepresentation
    = NoRepresentation

    | Set_Occurrence
    | Set_Explicit
    | Set_ExplicitVarSizeWithFlags
    | Set_ExplicitVarSizeWithMarker
    | Set_ExplicitVarSizeWithDummy

    | MSet_Occurrence
    | MSet_ExplicitWithFlags
    | MSet_ExplicitWithRepetition

    | Function_1D
    | Function_1DPartial
    | Function_ND
    | Function_NDPartial
    | Function_NDPartialDummy
    | Function_AsRelation HasRepresentation                     -- carries: representation for the inner relation

    | Sequence_ExplicitBounded

    | Relation_AsMatrix
    | Relation_AsSet HasRepresentation                          -- carries: representation for the inner set

    | Partition_AsSet HasRepresentation HasRepresentation       -- carries: representations for the inner sets
    | Partition_Occurrence
    | Permutation_AsFunction

    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize HasRepresentation
instance Hashable  HasRepresentation
instance ToJSON    HasRepresentation where toJSON = genericToJSON jsonOptions
instance FromJSON  HasRepresentation where parseJSON = genericParseJSON jsonOptions

instance Default HasRepresentation where
    def = NoRepresentation

representationConstrIndex :: HasRepresentation -> [Text]
representationConstrIndex r = oneLevel r : concatMap representationConstrIndex (children r)
    where
        oneLevel :: HasRepresentation -> Text
        oneLevel = stringToText . ("R"++) . show . constrIndex . toConstr

instance (Pretty r, Pretty a) => Pretty (Domain r a) where

    pretty DomainAny{} = "?"

    pretty DomainBool = "bool"

    pretty (DomainIntE t x) = "int" <> pretty t <> pretty t <> prParens (pretty x)

    -- print them like integers even when they are tagged
    -- pretty (DomainInt (TagEnum nm) _) = pretty nm
    -- pretty (DomainInt (TagUnnamed nm) _) = pretty nm

    pretty (DomainInt t []) = "int" <> pretty t

    pretty (DomainInt t ranges) = "int" <> pretty t <> prettyList prParens "," ranges

    pretty (DomainEnum name (Just ranges) _) = pretty name <> prettyList prParens "," ranges

    pretty (DomainEnum name _             _) = pretty name

    pretty (DomainUnnamed name _) = pretty name

    pretty (DomainTuple inners)
        = (if length inners < 2 then "tuple" else prEmpty)
        <+> prettyList prParens "," inners

    pretty (DomainRecord xs) = "record" <+> prettyList prBraces ","
        [ pretty nm <+> ":" <++> pretty d | (nm, d) <- xs ]

    pretty (DomainVariant xs) = "variant" <+> prettyList prBraces ","
        [ pretty nm <+> ":" <++> pretty d | (nm, d) <- xs ]

    pretty (DomainMatrix index innerNested)
        = "matrix indexed by" <+> prettyList prBrackets "," indices
                              <+> "of" <++> pretty inner
        where
            (indices,inner) = first (index:) $ collect innerNested
            collect (DomainMatrix i j) = first (i:) $ collect j
            collect x = ([],x)

    pretty (DomainSet r attrs inner) =
        "set" <+> prettyAttrs r attrs <+> "of" <++> pretty inner

    pretty (DomainMSet r attrs inner) =
        "mset" <+> prettyAttrs r attrs <+> "of" <++> pretty inner

    pretty (DomainFunction r attrs innerFrom innerTo) =
        "function" <+> prettyAttrs r attrs <++> pretty innerFrom <++> "-->" <++> pretty innerTo

    pretty (DomainSequence r attrs inner) =
        "sequence" <+> prettyAttrs r attrs <+> "of" <++> pretty inner

    pretty (DomainRelation r attrs inners)
        = "relation" <+> prettyAttrs r attrs <+> "of" <++> prettyList prParens " *" inners

    pretty (DomainPartition r attrs inner)
        = "partition" <+> prettyAttrs r attrs <+> "from" <++> pretty inner

    pretty (DomainPermutation r attrs inner) = hang ("permutation" <+> prettyAttrs r attrs <+> "of") 4 (pretty inner)

    pretty d@DomainOp{} = pretty (show d)

    pretty (DomainReference x _) = pretty x

    pretty (DomainMetaVar x) = "&" <> pretty x


prettyAttrs :: (Pretty a, Pretty b) => a -> b -> Doc
prettyAttrs a bs =
    let prettya = pretty a
    in  if show prettya == "()"
            then pretty bs
            else prBraces prettya <+> pretty bs

instance Pretty a => Pretty (DomainAttributes a) where
    pretty (DomainAttributes []) = prEmpty
    pretty (DomainAttributes attrs) = prettyList prParens "," attrs

instance Pretty a => Pretty (DomainAttribute a) where
    pretty (DAName name) = pretty name
    pretty (DANameValue name value) = pretty name <+> pretty value
    pretty DADotDot = ".."

instance Pretty a => Pretty (Range a) where
    pretty RangeOpen = ".."
    pretty (RangeSingle x) = pretty x
    pretty (RangeLowerBounded x) = pretty x <> ".."
    pretty (RangeUpperBounded x) = ".." <> pretty x
    pretty (RangeBounded x y) | show (pretty x) == show (pretty y) = pretty x
    pretty (RangeBounded x y) = pretty x <> ".." <> pretty y

instance Pretty HasRepresentation where
    pretty NoRepresentation = "∅"
    pretty r = pretty (representationToFullText r)

textToRepresentation :: Text -> [HasRepresentation] -> Maybe HasRepresentation
textToRepresentation t []             | t == "Occurrence"                 = return Set_Occurrence
textToRepresentation t []             | t == "Explicit"                   = return Set_Explicit
textToRepresentation t []             | t == "ExplicitVarSizeWithFlags"   = return Set_ExplicitVarSizeWithFlags
textToRepresentation t []             | t == "ExplicitVarSizeWithMarker"  = return Set_ExplicitVarSizeWithMarker
textToRepresentation t []             | t == "ExplicitVarSizeWithDummy"   = return Set_ExplicitVarSizeWithDummy
textToRepresentation t []             | t == "MOccurrence"                = return MSet_Occurrence
textToRepresentation t []             | t == "ExplicitWithFlags"          = return MSet_ExplicitWithFlags
textToRepresentation t []             | t == "ExplicitWithRepetition"     = return MSet_ExplicitWithRepetition
textToRepresentation t []             | t == "Function1D"                 = return Function_1D
textToRepresentation t []             | t == "Function1DPartial"          = return Function_1DPartial
textToRepresentation t []             | t == "FunctionND"                 = return Function_ND
textToRepresentation t []             | t == "FunctionNDPartial"          = return Function_NDPartial
textToRepresentation t []             | t == "FunctionNDPartialDummy"     = return Function_NDPartialDummy
textToRepresentation t [repr]         | t == "FunctionAsRelation"         = return (Function_AsRelation repr)
textToRepresentation t []             | t == "ExplicitBounded"            = return Sequence_ExplicitBounded
textToRepresentation t []             | t == "RelationAsMatrix"           = return Relation_AsMatrix
textToRepresentation t [repr]         | t == "RelationAsSet"              = return (Relation_AsSet repr)
textToRepresentation t [repr1, repr2] | t == "PartitionAsSet"             = return (Partition_AsSet repr1 repr2)
textToRepresentation t []             | t == "PartitionOccurrence"        = return Partition_Occurrence
textToRepresentation _ _ = Nothing

representationToShortText :: HasRepresentation -> Text
representationToShortText Set_Occurrence                 = "Occurrence"
representationToShortText Set_Explicit                   = "Explicit"
representationToShortText Set_ExplicitVarSizeWithFlags   = "ExplicitVarSizeWithFlags"
representationToShortText Set_ExplicitVarSizeWithMarker  = "ExplicitVarSizeWithMarker"
representationToShortText Set_ExplicitVarSizeWithDummy   = "ExplicitVarSizeWithDummy"
representationToShortText MSet_Occurrence                = "MOccurrence"
representationToShortText MSet_ExplicitWithFlags         = "ExplicitWithFlags"
representationToShortText MSet_ExplicitWithRepetition    = "ExplicitWithRepetition"
representationToShortText Function_1D                    = "Function1D"
representationToShortText Function_1DPartial             = "Function1DPartial"
representationToShortText Function_ND                    = "FunctionND"
representationToShortText Function_NDPartial             = "FunctionNDPartial"
representationToShortText Function_NDPartialDummy        = "FunctionNDPartialDummy"
representationToShortText Function_AsRelation{}          = "FunctionAsRelation"
representationToShortText Sequence_ExplicitBounded       = "ExplicitBounded"
representationToShortText Relation_AsMatrix              = "RelationAsMatrix"
representationToShortText Relation_AsSet{}               = "RelationAsSet"
representationToShortText Partition_AsSet{}              = "PartitionAsSet"
representationToShortText Partition_Occurrence           = "PartitionOccurrence"
representationToShortText Permutation_AsFunction        = "PermutationAsFunction"
representationToShortText r = bug ("representationToShortText:" <+> pretty (show r))

representationToFullText :: HasRepresentation -> Text
representationToFullText (Function_AsRelation repr)     = mconcat [ "FunctionAsRelation"
                                                                  , "["
                                                                  , representationToFullText repr
                                                                  , "]"
                                                                  ]
representationToFullText (Relation_AsSet repr)          = mconcat [ "RelationAsSet"
                                                                  , "["
                                                                  , representationToFullText repr
                                                                  , "]"
                                                                  ]
representationToFullText (Partition_AsSet repr1 repr2)  = mconcat [ "PartitionAsSet"
                                                                  , "["
                                                                  , representationToFullText repr1
                                                                  , ","
                                                                  , representationToFullText repr2
                                                                  , "]"
                                                                  ]
representationToFullText r = representationToShortText r


normaliseDomain :: (Ord c, ExpressionLike c) => (c -> c) -> Domain r c -> Domain r c
normaliseDomain _norm DomainBool                  = DomainBool
normaliseDomain  norm (DomainInt t rs           ) = DomainInt t $ sort $ map (normaliseRange norm) (expandRanges rs)
normaliseDomain _norm (DomainEnum n Nothing   mp) = DomainEnum n Nothing mp
normaliseDomain _norm (DomainEnum n (Just rs) mp) = DomainEnum n (Just $ sort rs) mp
normaliseDomain  norm (DomainUnnamed n x        ) = DomainUnnamed n (norm x)
normaliseDomain  norm (DomainRecord           doms     ) = DomainRecord  [ (n, normaliseDomain norm d)
                                                                         | (n, d) <- doms ]
normaliseDomain  norm (DomainVariant          doms     ) = DomainVariant [ (n, normaliseDomain norm d)
                                                                         | (n, d) <- doms ]
normaliseDomain  norm (DomainTuple            doms     ) = DomainTuple $ map (normaliseDomain norm) doms
normaliseDomain  norm (DomainMatrix           dom1 dom2) = DomainMatrix      (normaliseDomain norm dom1)
                                                                             (normaliseDomain norm dom2)
normaliseDomain  norm (DomainSet       r attr dom      ) = DomainSet       r (fmap norm attr)
                                                                             (normaliseDomain norm dom)
normaliseDomain  norm (DomainMSet      r attr dom      ) = DomainMSet      r (fmap norm attr)
                                                                             (normaliseDomain norm dom)
normaliseDomain  norm (DomainFunction  r attr dom1 dom2) = DomainFunction  r (fmap norm attr)
                                                                             (normaliseDomain norm dom1)
                                                                             (normaliseDomain norm dom2)
normaliseDomain  norm (DomainSequence  r attr dom      ) = DomainSequence  r (fmap norm attr)
                                                                             (normaliseDomain norm dom)
normaliseDomain  norm (DomainRelation  r attr doms     ) = DomainRelation  r (fmap norm attr)
                                                                             (map (normaliseDomain norm) doms)
normaliseDomain  norm (DomainPartition r attr dom      ) = DomainPartition r (fmap norm attr)
                                                                             (normaliseDomain norm dom)
normaliseDomain _norm d = d

normaliseRange :: (c -> c) -> Range c -> Range c
normaliseRange _norm RangeOpen             = RangeOpen
normaliseRange  norm (RangeSingle x)       = RangeBounded (norm x) (norm x)
normaliseRange  norm (RangeLowerBounded x) = RangeLowerBounded (norm x)
normaliseRange  norm (RangeUpperBounded x) = RangeUpperBounded (norm x)
normaliseRange  norm (RangeBounded x y)    = RangeBounded (norm x) (norm y)

innerDomainOf :: (MonadFailDoc m, Show x) => Domain () x -> m (Domain () x)
innerDomainOf (DomainMatrix _ t) = return t
innerDomainOf (DomainSet _ _ t) = return t
innerDomainOf (DomainMSet _ _ t) = return t
innerDomainOf (DomainFunction _ _ a b) = return (DomainTuple [a,b])
innerDomainOf (DomainRelation _ _ ts) = return (DomainTuple ts)
innerDomainOf (DomainPartition _ _ t) = return (DomainSet () def t)
innerDomainOf t = failDoc ("innerDomainOf:" <+> pretty (show t))

singletonDomainInt :: (Eq x, CanBeAnAlias x) => Domain r x -> Maybe x
singletonDomainInt (DomainInt _ [RangeSingle a]) = Just a
singletonDomainInt (DomainInt _ [RangeBounded a b]) =
    let
        followAlias (isAlias -> Just x) = followAlias x
        followAlias x = x
    in
        if followAlias a == followAlias b
            then Just a
            else Nothing
singletonDomainInt _ = Nothing

matrixNumDimsD :: Domain r x -> Int
matrixNumDimsD (DomainMatrix _ t) = 1 + matrixNumDimsD t
matrixNumDimsD _ = 0

