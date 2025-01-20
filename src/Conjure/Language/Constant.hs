{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Conjure.Language.Constant
    ( Constant(..)
    , valuesInIntDomain
    , normaliseConstant
    , mkUndef, isUndef
    , emptyCollection
    , viewConstantBool
    , viewConstantInt
    , viewConstantIntWithTag
    , viewConstantTuple
    , viewConstantRecord
    , viewConstantVariant
    , viewConstantMatrix
    , viewConstantSet
    , viewConstantMSet
    , viewConstantFunction
    , viewConstantSequence
    , viewConstantRelation
    , viewConstantPartition
    , viewConstantPermutation
    , reDomConst
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.AbstractLiteral

import Conjure.Language.DomainSizeOf
import Conjure.Language.TypeOf
import Conjure.Language.AdHoc
import Conjure.Language.Pretty

-- base
import Data.Data ( toConstr, constrIndex )

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), oneof )

-- aeson
import qualified Data.Aeson as JSON
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data Constant
    = ConstantBool Bool
    | ConstantInt IntTag Integer
    | ConstantEnum Name   {- name for the enum domain -}
                   [Name] {- values in the enum domain -}
                   Name   {- the literal -}
    | ConstantField Name Type                               -- the name of a field of Record or Variant and its type
    | ConstantAbstract (AbstractLiteral Constant)
    | DomainInConstant (Domain () Constant)
    | TypedConstant Constant Type
    | ConstantUndefined Text Type                           -- never use this for a bool
                                                            -- use false instead for them
    deriving (Show, Data, Typeable, Generic)

instance Eq Constant where
    a == b = compare a b == EQ

-- implementing the Eq&Ord instances by hand, because we want to special case the TypedConstant constructor
instance Ord Constant where

    -- do not use type info when comparing
    compare (TypedConstant a _) (TypedConstant b _) = compare a b
    compare (TypedConstant a _) b = compare a b
    compare a (TypedConstant b _) = compare a b

    -- the "usual" comparisons
    compare (ConstantBool a) (ConstantBool b) = compare a b
    compare (ConstantInt _ a) (ConstantInt _ b) = compare a b
    compare (ConstantEnum _ aVals aVal) (ConstantEnum _ bVals bVal) =
        compare (elemIndex aVal aVals, aVal) (elemIndex bVal bVals, bVal)
    compare (ConstantField a1 a2) (ConstantField b1 b2) = compare (a1,a2) (b1,b2)
    compare (ConstantAbstract a) (ConstantAbstract b) = compare a b
    compare (DomainInConstant a) (DomainInConstant b) = compare a b
    compare (ConstantUndefined a1 a2) (ConstantUndefined b1 b2) = compare (a1,a2) (b1,b2)

    -- if the constructors do not match
    compare a b = compare (constrIndex (toConstr a)) (constrIndex (toConstr b))

instance Serialize Constant
instance Hashable  Constant
instance ToJSON    Constant where toJSON = genericToJSON jsonOptions
instance FromJSON  Constant where parseJSON = genericParseJSON jsonOptions

instance SimpleJSON Constant where
    toSimpleJSON c =
        case c of
            ConstantBool b -> return (toJSON b)
            ConstantInt _ i -> return (toJSON i)
            ConstantEnum _ _ nm -> return (toJSON (renderNormal nm))
            ConstantAbstract lit -> toSimpleJSON lit
            TypedConstant c' _ -> toSimpleJSON c'
            _ -> noToSimpleJSON c

    fromSimpleJSON _ (JSON.Bool b) = return (ConstantBool b)

    fromSimpleJSON (TypeInt (TagEnum enum_type_name)) (JSON.String value) =
        return (ConstantEnum (Name enum_type_name) [] (Name value))

    fromSimpleJSON t@TypeInt{} x@JSON.Number{} = ConstantInt TagInt <$> fromSimpleJSON t x
    fromSimpleJSON t@TypeInt{} x@JSON.String{} = ConstantInt TagInt <$> fromSimpleJSON t x

    fromSimpleJSON (TypeEnum enum_type_name) (JSON.String value) =
        return (ConstantEnum enum_type_name [] (Name value))

    fromSimpleJSON (TypeTuple ts) (JSON.Array xs) =
        ConstantAbstract . AbsLitTuple <$> zipWithM fromSimpleJSON ts (V.toList xs)

    fromSimpleJSON t@(TypeVariant ts) x@(JSON.Object m) = do
        mys <- forM (KM.toList m) $ \ (toText->name, value) -> do
            let mty = [ ty | (nm, ty) <- ts, nm == Name name ]
            case mty of
                [ty] -> do
                    value' <- fromSimpleJSON ty value
                    return $ Just $ ConstantAbstract $ AbsLitVariant Nothing (Name name) value'
                _ -> return Nothing
        let ys = catMaybes mys
        case ys of
            [y] -> return y
            _ -> noFromSimpleJSON "Constant" t x

    fromSimpleJSON t@(TypeRecord ts) x@(JSON.Object m) = do
        mys <- forM (KM.toList m) $ \ (toText->name, value) -> do
            let mty = [ ty | (nm, ty) <- ts, nm == Name name ]
            case mty of
                [ty] -> do
                    value' <- fromSimpleJSON ty value
                    return $ Just (Name name, value')
                _ -> return Nothing
        let ys = catMaybes mys
        if length ys == length mys
            then return $ ConstantAbstract $ AbsLitRecord ys
            else noFromSimpleJSON "Constant" t x

    fromSimpleJSON (TypeMatrix index inner) (JSON.Object m) = do
        ys <- forM (KM.toList m) $ \ (toText->name, value) -> do
            -- the name must be an integer
            a <- fromSimpleJSON index (JSON.String name)
            b <- fromSimpleJSON inner value
            return (a, b)
        -- traceM $ show ys
        -- traceM $ show $ sort ys

        let ys_sorted = sort ys
        let domain_ints = map fst ys_sorted
        let domain = if maximum domain_ints - minimum domain_ints + 1 == genericLength domain_ints
                        then DomainInt TagInt [RangeBounded (ConstantInt TagInt $ minimum domain_ints) (ConstantInt TagInt $ maximum domain_ints)]
                        else DomainInt TagInt (map (RangeSingle . ConstantInt TagInt) domain_ints)

        return $ ConstantAbstract $ AbsLitMatrix domain (map snd ys_sorted)

    fromSimpleJSON (TypeMatrix _index inner) (JSON.Array xs) =
        let domain = DomainInt TagInt [RangeBounded 1 (fromInt $ genericLength $ V.toList xs)] in
        ConstantAbstract . AbsLitMatrix domain <$> mapM (fromSimpleJSON inner) (V.toList xs)

    fromSimpleJSON (TypeSet t) (JSON.Array xs) =
        ConstantAbstract . AbsLitSet <$> mapM (fromSimpleJSON t) (V.toList xs)

    fromSimpleJSON (TypeMSet t) (JSON.Array xs) =
        ConstantAbstract . AbsLitMSet <$> mapM (fromSimpleJSON t) (V.toList xs)

    fromSimpleJSON (TypeFunction fr to) (JSON.Object m) = do
        ys <- forM (KM.toList m) $ \ (toText->name, value) -> do
            -- the name must be an integer
            -- and this is a function from ints we are reading here
            a <- fromSimpleJSON fr (JSON.String name)
            b <- fromSimpleJSON to value
            return (a, b)
        return $ ConstantAbstract $ AbsLitFunction ys

    fromSimpleJSON ty@(TypeFunction fr to) value@(JSON.Array xs) = do
        mys <- forM (V.toList xs) $ \case
                    JSON.Array x' ->
                        case V.toList x' of
                            [a', b'] -> do
                                a <- fromSimpleJSON fr a'
                                b <- fromSimpleJSON to b'
                                return $ Just (a, b)
                            _ -> return Nothing
                    _ -> return Nothing
        let ys = catMaybes mys
        if length ys == length mys
            then return $ ConstantAbstract $ AbsLitFunction ys
            else noFromSimpleJSON "Constant" ty value

    fromSimpleJSON (TypeSequence inner) (JSON.Object m) = do
        ys :: [(Integer, Constant)] <- forM (KM.toList m) $ \ (toText->name, value) -> do
            -- the name must be an integer
            a <- fromSimpleJSON (TypeInt TagInt) (JSON.String name)
            b <- fromSimpleJSON inner value
            return (a, b)

        let ys_sorted = sort ys

        return $ ConstantAbstract $ AbsLitSequence (map snd ys_sorted)

    fromSimpleJSON (TypeSequence t) (JSON.Array xs) =
        ConstantAbstract . AbsLitSequence <$> mapM (fromSimpleJSON t) (V.toList xs)

    fromSimpleJSON ty@(TypeRelation ts) value@(JSON.Array xs) = do
        minners <- forM (V.toList xs) $ \ x -> do
            mtuple <- fromSimpleJSON (TypeTuple ts) x
            case mtuple of
                ConstantAbstract (AbsLitTuple tuple) -> return (Just tuple)
                _ -> return Nothing
        let inners = catMaybes minners
        if length inners == length minners
            then return $ ConstantAbstract $ AbsLitRelation inners
            else noFromSimpleJSON "Constant" ty value
        

    -- fromSimpleJSON _ (JSON.String s) = return $ ConstantEnum (Name "<unknown>") [] (Name s)
    -- -- fromSimpleJSON _ (JSON.Array xs) = do
    -- --     ys <- mapM fromSimpleJSON (V.toList xs)
    -- --     return $ ConstantFromJSON ys
    -- fromSimpleJSON t (JSON.Object m) = do
    --     traceM $ show $ "fromSimpleJSON.Constant type" <+> pretty t
    --     traceM $ show $ "fromSimpleJSON.Constant type" <+> pretty (show t)
    --     ys <- forM (M.toList m) $ \ (name, value) ->
    --         -- the name must be an integer
    --         -- and this is a function from ints we are reading here
    --         case readMay (textToString name) of
    --             Nothing -> userErr1 $ vcat [ "This is not an int. Boo.", pretty name, pretty value]
    --             Just a -> do
    --                 b <- fromSimpleJSON t value
    --                 return (ConstantInt TagInt a, b)
    --     return $ ConstantAbstract $ AbsLitFunction ys
    fromSimpleJSON t x = noFromSimpleJSON "Constant" t x

instance ToFromMiniZinc Constant where
    toMiniZinc c =
        case c of
            ConstantBool b -> return (MZNBool b)
            ConstantInt _ i -> return (MZNInt i)
            ConstantAbstract lit -> toMiniZinc lit
            TypedConstant c' _ -> toMiniZinc c'
            _ -> noToMiniZinc c

instance Arbitrary Constant where
    arbitrary = oneof
        [ ConstantBool <$> arbitrary
        , ConstantInt TagInt <$> arbitrary
        ]

instance TypeOf Constant where
    typeOf ConstantBool{}             = return TypeBool
    typeOf (ConstantInt t _)          = return (TypeInt t)
    typeOf (ConstantEnum defn _ _ )   = return (TypeEnum defn)
    typeOf (ConstantField _ ty)       = return ty
    typeOf (ConstantAbstract x    )   = typeOf x
    typeOf (DomainInConstant dom)     = typeOfDomain dom
    typeOf (TypedConstant _ ty)       = return ty
    typeOf (ConstantUndefined _ ty)   = return ty

instance DomainSizeOf Constant Integer where
    domainSizeOf DomainBool{} = return 2
    domainSizeOf (DomainIntE _ x) = bug ("not implemented, domainSizeOf DomainIntE" <+> pretty (show x))
    domainSizeOf (DomainInt _ rs) = domainSizeOfRanges rs
    domainSizeOf DomainEnum{} = failDoc  "domainSizeOf: Unknown for given enum."
    domainSizeOf (DomainTuple ds) = product <$> mapM domainSizeOf ds
    domainSizeOf (DomainMatrix index inner) = intPow <$> domainSizeOf inner <*> domainSizeOf index
    domainSizeOf d@(DomainSet _ (SetAttr attrs) inner) =
        case attrs of
            SizeAttr_None -> do
                innerSize <- domainSizeOf inner
                return (2 `intPow` innerSize)
            SizeAttr_Size (ConstantInt _ size) -> do
                innerSize <- domainSizeOf inner
                return (nchoosek (product . enumFromTo 1) innerSize size)
            SizeAttr_MinSize{} -> do
                -- TODO: we can do better here
                innerSize <- domainSizeOf inner
                return (2 `intPow` innerSize)
            SizeAttr_MaxSize (ConstantInt _ maxSize) -> do
                innerSize <- domainSizeOf inner
                return $ sum [ nchoosek (product . enumFromTo 1) innerSize k | k <- [0 .. maxSize] ]
            SizeAttr_MinMaxSize (ConstantInt _ minSize) (ConstantInt _ maxSize) -> do
                innerSize <- domainSizeOf inner
                return $ sum [ nchoosek (product . enumFromTo 1) innerSize k | k <- [minSize .. maxSize] ]
            _ -> failDoc  ("domainSizeOf{Constant}" <+> pretty d)
    domainSizeOf DomainMSet      {} = bug "not implemented: domainSizeOf DomainMSet"
    domainSizeOf DomainFunction  {} = bug "not implemented: domainSizeOf DomainFunction"
    domainSizeOf DomainRelation  {} = bug "not implemented: domainSizeOf DomainRelation"
    domainSizeOf DomainPartition {} = bug "not implemented: domainSizeOf DomainPartition"
    domainSizeOf _                  = bug "not implemented: domainSizeOf"

emptyCollection :: Constant -> Bool
emptyCollection ConstantBool{} = False
emptyCollection ConstantInt{} = False
emptyCollection ConstantEnum{} = False
emptyCollection ConstantField{} = False
emptyCollection (ConstantAbstract x) = emptyCollectionAbsLit x
emptyCollection DomainInConstant{} = False
emptyCollection (TypedConstant x _) = emptyCollection x
emptyCollection ConstantUndefined{} = False

intPow :: Integer -> Integer -> Integer
intPow = (^)

domainSizeOfRanges :: MonadFailDoc m => [Range Constant] -> m Integer
domainSizeOfRanges = fmap genericLength . valuesInIntDomain

instance DomainSizeOf Constant Constant where
    domainSizeOf = fmap (ConstantInt TagInt) . domainSizeOf

instance Pretty Constant where

    pretty (TypedConstant (ConstantAbstract (AbsLitMatrix _ [])) ty) | TypeAny `elem` universe ty = "[]"

    -- hack, oh sweet hack!
    -- print a domain instead of a type when printing an empty matrix literal.
    -- this means we print "int()" instead of "int" inside the index of a matrix type
    -- SR expects it this way...
    pretty (TypedConstant (ConstantAbstract (AbsLitMatrix _ [])) ty) =
        let
            pretty' (TypeMatrix index innerNested)
                = "matrix indexed by" <+> prettyList prBrackets "," (map pretty' indices)
                                      <+> "of" <+> pretty inner
                where
                    (indices,inner) = first (index:) $ collect innerNested
                    collect (TypeMatrix i j) = first (i:) $ collect j
                    collect x = ([],x)
            pretty' TypeInt{} = "int()"
            pretty' t = pretty t
        in
            prParens $ "[] : `" <> pretty' ty <> "`"

    pretty (ConstantBool False)          = "false"
    pretty (ConstantBool True )          = "true"
    pretty (ConstantInt t x)             = pretty x <> pretty t
    pretty (ConstantEnum _ _ x)          = pretty x
    pretty (ConstantField n _)           = pretty n
    pretty (ConstantAbstract x)          = pretty x
    pretty (DomainInConstant d)          = "`" <> pretty d <> "`"
    pretty (TypedConstant x ty)          = prParens $ pretty x <+> ":" <+> "`" <> pretty ty <> "`"
    pretty (ConstantUndefined reason ty) = "undefined" <> prParens (pretty reason <+> ":" <+> "`" <> pretty ty <> "`")

instance ExpressionLike Constant where
    fromInt = ConstantInt TagInt
    fromIntWithTag i t = ConstantInt t i
    intOut _ (ConstantInt _ x) = return x
    intOut doc c = failDoc  $ vcat [ "Expecting an integer, but found:" <+> pretty c
                               , "Called from:" <+> doc
                               ]

    fromBool = ConstantBool
    boolOut (ConstantBool x) = return x
    boolOut ConstantUndefined{} = return False
    boolOut c = failDoc  ("Expecting a boolean, but found:" <+> pretty c)

    fromList xs = ConstantAbstract $ AbsLitMatrix (mkDomainIntB 1 (fromInt $ genericLength xs)) xs
    listOut (ConstantAbstract (AbsLitMatrix _ xs)) = return xs
    listOut c = failDoc  ("Expecting a matrix literal, but found:" <+> pretty c)

instance ReferenceContainer Constant where
    fromName name = bug ("ReferenceContainer{Constant} fromName --" <+> pretty name)
    nameOut (ConstantField nm _) = return nm
    nameOut p = bug ("ReferenceContainer{Constant} nameOut --" <+> pretty p)

instance DomainContainer Constant (Domain ()) where
    fromDomain = DomainInConstant
    domainOut (DomainInConstant dom) = return dom
    domainOut _ = failDoc  "domainOut{Constant}"

mkUndef :: Type -> Doc -> Constant
mkUndef TypeBool _ = ConstantBool False
mkUndef ty reason = ConstantUndefined (stringToText $ show reason) ty

isUndef :: Constant -> Bool
isUndef ConstantUndefined{} = True
isUndef _ = False

normaliseConstant :: Constant -> Constant
normaliseConstant x@ConstantBool{} = x
normaliseConstant x@ConstantInt{}  = x
normaliseConstant x@ConstantEnum{} = x
normaliseConstant x@ConstantField{} = x
normaliseConstant (ConstantAbstract x) = ConstantAbstract (normaliseAbsLit normaliseConstant x)
normaliseConstant (DomainInConstant d) = DomainInConstant (normaliseDomain normaliseConstant d)
normaliseConstant (TypedConstant c ty) = TypedConstant (normaliseConstant c) ty
normaliseConstant x@ConstantUndefined{} = x

instance Num Constant where
    ConstantInt _ x + ConstantInt _ y = ConstantInt TagInt (x+y)
    x + y = bug $ vcat [ "Num Constant (+)", "x:" <+> pretty x, "y:" <+> pretty y ]
    ConstantInt _ x - ConstantInt _ y = ConstantInt TagInt (x-y)
    x - y = bug $ vcat [ "Num Constant (-)", "x:" <+> pretty x, "y:" <+> pretty y ]
    ConstantInt _ x * ConstantInt _ y = ConstantInt TagInt (x*y)
    x * y = bug $ vcat [ "Num Constant (*)", "x:" <+> pretty x, "y:" <+> pretty y ]
    abs (ConstantInt t x) = ConstantInt t (abs x)
    abs x = bug $ vcat [ "Num Constant abs", "x:" <+> pretty x ]
    signum (ConstantInt t x) = ConstantInt t (signum x)
    signum x = bug $ vcat [ "Num Constant signum", "x:" <+> pretty x ]
    fromInteger = ConstantInt TagInt . fromInteger


valuesInIntDomain :: MonadFailDoc m => [Range Constant] -> m [Integer]
valuesInIntDomain ranges =
    if isFinite
        then return allValues
        else failDoc  $ "Expected finite integer ranges, but got:" <++> prettyList id "," ranges

    where

        allRanges :: [Maybe [Integer]]
        allRanges =
            [ vals
            | r <- ranges
            , let vals = case r of
                    RangeSingle (ConstantInt _ x) -> return [x]
                    RangeBounded (ConstantInt _ l) (ConstantInt _ u) -> return [l..u]
                    _ -> Nothing
            ]

        isFinite :: Bool
        isFinite = Nothing `notElem` allRanges

        allValues :: [Integer]
        allValues = sortNub $ concat $ catMaybes allRanges


viewConstantBool :: MonadFailDoc m => Constant -> m Bool
viewConstantBool (ConstantBool i) = return i
viewConstantBool (ConstantInt _ 0) = return False
viewConstantBool (ConstantInt _ 1) = return True
viewConstantBool constant = failDoc  ("Expecting a boolean, but got:" <++> pretty constant)

viewConstantInt :: MonadFailDoc m => Constant -> m Integer
viewConstantInt (ConstantInt _ i) = return i
viewConstantInt constant = failDoc  ("Expecting an integer, but got:" <++> pretty constant)

viewConstantIntWithTag :: MonadFailDoc m => Constant -> m (IntTag, Integer)
viewConstantIntWithTag (ConstantInt t i) = return (t, i)
viewConstantIntWithTag constant = failDoc  ("Expecting an integer, but got:" <++> pretty constant)

viewConstantTuple :: MonadFailDoc m => Constant -> m [Constant]
viewConstantTuple (ConstantAbstract (AbsLitTuple xs)) = return xs
viewConstantTuple (TypedConstant c _) = viewConstantTuple c
viewConstantTuple constant = failDoc  ("Expecting a tuple, but got:" <++> pretty constant)

viewConstantRecord :: MonadFailDoc m => Constant -> m [(Name, Constant)]
viewConstantRecord (ConstantAbstract (AbsLitRecord xs)) = return (sortOn fst xs)
viewConstantRecord (TypedConstant c _) = viewConstantRecord c
viewConstantRecord constant = failDoc  ("Expecting a record, but got:" <++> pretty constant)

viewConstantVariant :: MonadFailDoc m => Constant -> m (Maybe [(Name, Domain () Constant)], Name, Constant)
viewConstantVariant (ConstantAbstract (AbsLitVariant lu nm x)) = return (lu, nm, x)
viewConstantVariant (TypedConstant c _) = viewConstantVariant c
viewConstantVariant constant = failDoc  ("Expecting a variant, but got:" <++> pretty constant)

viewConstantMatrix :: MonadFailDoc m => Constant -> m (Domain () Constant, [Constant])
viewConstantMatrix (ConstantAbstract (AbsLitMatrix ind xs)) = return (expandDomainReference ind, xs)
viewConstantMatrix (TypedConstant c _) = viewConstantMatrix c
viewConstantMatrix constant =
    case viewConstantFunction constant of
        Nothing -> failDoc ("Expecting a matrix, but got:" <++> pretty constant)
        Just func -> do
            let indices = map fst func
                values = map snd func
                indices_as_int = [ i | ConstantInt _ i <- indices ]
            if length indices == length indices_as_int
                then
                    if not (null indices)
                        then
                            if maximum indices_as_int - minimum indices_as_int + 1 == genericLength indices
                                then return (DomainInt TagInt [RangeBounded (fromInt (minimum indices_as_int)) (fromInt (maximum indices_as_int))], values)
                                else return (DomainInt TagInt (map (RangeSingle . fromInt) indices_as_int), values)
                        else
                            return (DomainInt TagInt [RangeBounded 1 0], values)
                else
                    failDoc ("Expecting a matrix, but got:" <++> pretty constant)

viewConstantSet :: MonadFailDoc m => Constant -> m [Constant]
viewConstantSet (ConstantAbstract (AbsLitSet xs)) = return xs
viewConstantSet (TypedConstant c _) = viewConstantSet c
viewConstantSet constant = failDoc  ("Expecting a set, but got:" <++> pretty constant)

viewConstantMSet :: MonadFailDoc m => Constant -> m [Constant]
viewConstantMSet (ConstantAbstract (AbsLitMSet xs)) = return xs
viewConstantMSet (TypedConstant c _) = viewConstantMSet c
viewConstantMSet constant = failDoc ("Expecting an mset, but got:" <++> pretty constant)

viewConstantFunction :: MonadFailDoc m => Constant -> m [(Constant, Constant)]
viewConstantFunction (ConstantAbstract (AbsLitFunction xs)) = return xs
viewConstantFunction (TypedConstant c _) = viewConstantFunction c
viewConstantFunction constant = do
    let
        suggestion = case constant of
            ConstantAbstract (AbsLitMatrix (expandDomainReference -> DomainInt _ rs) vals) -> do
                froms <- valuesInIntDomain rs
                return $ Just $ pretty $ AbsLitFunction (zip (map (ConstantInt TagInt) froms) vals)
            _ -> return Nothing
    suggestion >>= \case
        Nothing  -> failDoc ("Expecting a function, but got:" <++> pretty constant)
        Just sug -> failDoc (vcat [ "Expecting a function, but got:" <++> pretty constant
                               , "Maybe you meant:" <++> sug
                               ])

viewConstantSequence :: MonadFailDoc m => Constant -> m [Constant]
viewConstantSequence (ConstantAbstract (AbsLitSequence xs)) = return xs
viewConstantSequence (ConstantAbstract (AbsLitMatrix _ xs)) = return xs
viewConstantSequence (TypedConstant c _) = viewConstantSequence c
viewConstantSequence constant = failDoc ("Expecting a sequence, but got:" <++> pretty constant)

viewConstantRelation :: MonadFailDoc m => Constant -> m [[Constant]]
viewConstantRelation (ConstantAbstract (AbsLitRelation xs)) = return xs
viewConstantRelation (TypedConstant c _) = viewConstantRelation c
viewConstantRelation constant = failDoc ("Expecting a relation, but got:" <++> pretty constant)

viewConstantPartition :: MonadFailDoc m => Constant -> m [[Constant]]
viewConstantPartition (ConstantAbstract (AbsLitPartition xs)) = return xs
viewConstantPartition (TypedConstant c _) = viewConstantPartition c
viewConstantPartition constant = failDoc ("Expecting a partition, but got:" <++> pretty constant)

viewConstantPermutation :: MonadFailDoc m => Constant -> m [[Constant]]
viewConstantPermutation (ConstantAbstract (AbsLitPermutation xs)) = return xs
viewConstantPermutation (TypedConstant c _) = viewConstantPermutation c
viewConstantPermutation constant = failDoc ("Expecting a permutation, but got:" <++> pretty constant)


reDomConst :: Domain () Constant -> Domain () Constant 
reDomConst cns = case cns of
                   DomainInt t _ -> reTag t cns
                   _ -> cns

