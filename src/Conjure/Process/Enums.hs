{-# LANGUAGE TupleSections #-}

module Conjure.Process.Enums
    ( removeEnumsFromModel
    , removeEnumsFromParam
    , addEnumsAndUnnamedsBack
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.Pretty
import Conjure.Language.Type

-- text
import Data.Text as T ( pack )

-- unordered-containers
import qualified Data.HashMap.Strict as M


-- | The argument is a model before nameResolution.
--   Only intended to work on problem specifications.
removeEnumsFromModel ::
    MonadFailDoc m =>
    MonadLog m =>
    MonadUserError m =>
    Model -> m Model
removeEnumsFromModel =
    preCheckForNameReuse >=>
    removeEnumsFromModel_LettingEnums >=>
    removeEnumsFromModel_GivenEnums   >=>
    checkEnums

    where

        -- check if names defined as part of enumerated types are later used as names of top-level or quantified declarations
        preCheckForNameReuse model = do
            let enumNames = concat [ names | Declaration (LettingDomainDefnEnum _ names) <- mStatements model ]
            let redefinedTopLevel = [ name | Declaration (FindOrGiven _ name _) <- mStatements model, name `elem` enumNames ]
            let redefinedQuantified = [ name | Generator gen <- universeBi (mStatements model)
                                             , name@Name{} <- case gen of
                                                        GenDomainNoRepr defn _ -> universeBi defn
                                                        GenDomainHasRepr defn _ -> universeBi defn
                                                        GenInExpr defn _ -> universeBi defn
                                             , name `elem` enumNames ]
            let redefined = redefinedTopLevel ++ redefinedQuantified
            let duplicates = [ name | (name, count) <- histogram enumNames, count > 1 ]
            unless (null duplicates) $ userErr1 $ "Enumerated value defined multiple times:" <+> prettyList id "," duplicates
            unless (null redefined) $ userErr1 $ vcat
                [ "Members of an enum domain are later redefined as top-level or quantified variables."
                , "Check:" <+> prettyList id "," redefined
                ]
            return model

        removeEnumsFromModel_LettingEnums model = do
            (statements', ( enumDomainNames :: [(Name, Domain () Expression)]
                          , nameToIntMapping_ :: [(Name, (Name, Integer))]
                          )) <-
                flip runStateT ([], []) $ forM (mStatements model) $ \ st ->
                    case st of
                        Declaration (LettingDomainDefnEnum ename@(Name enameText) names) -> do
                            namesBefore <- gets (map fst . snd)
                            let outDomain = mkDomainIntBTagged (TagEnum enameText)
                                                (fromIntWithTag 1 (TagEnum enameText))
                                                (fromIntWithTag (genericLength names) (TagEnum enameText))
                            case names `intersect` namesBefore of
                                [] -> modify ( ( [(ename, outDomain)]
                                             , zip names (map (ename,) allNats)
                                             ) `mappend` )
                                repeated -> userErr1 $ vcat
                                    [ "Some members of this enum domain (" <> pretty ename <> ") seem to be defined"
                                    , "as part of other enum domains."
                                    , "Repeated:" <+> prettyList id "," repeated
                                    , "While working on domain:" <+> pretty st
                                    ]
                            return [ Declaration (Letting (ename `mappend` "_EnumSize") (fromInt $ genericLength names))
                                   , Declaration (Letting ename (Domain outDomain))
                                   ]
                        _ -> return [st]

            let nameToIntMapping = M.fromList nameToIntMapping_

            let
                onX :: Monad m => Expression -> m Expression
                onX (Reference nm Nothing)
                    | Just (Name ename, i) <- M.lookup nm nameToIntMapping
                    = return (fromIntWithTag i (TagEnum ename))
                onX p = return p

                onD :: MonadFailDoc m => Domain () Expression -> m (Domain () Expression)
                onD (DomainEnum nm@(Name nmText) (Just ranges) _)
                    | Just _ <- lookup nm enumDomainNames
                    = DomainInt (TagEnum nmText) <$> mapM (mapM (nameToX nameToIntMapping)) ranges
                onD (DomainEnum nm Nothing _)
                    | Just d <- lookup nm enumDomainNames
                    = return (DomainReference nm (Just d))
                onD (DomainReference nm Nothing)
                    | Just d <- lookup nm enumDomainNames
                    = return (DomainReference nm (Just d))
                onD p = return p

            statements'' <- (transformBiM onD >=> transformBiM onX) statements'
            return model { mStatements = concat statements'' }

        removeEnumsFromModel_GivenEnums model = do
            (statements', enumDomainNames) <-
                flip runStateT [] $ forM (mStatements model) $ \ st ->
                    case st of
                        Declaration (GivenDomainDefnEnum name@(Name nameText)) -> do
                            let nameS      = name `mappend` "_EnumSize"
                            let outDomainS = DomainInt (TagEnum nameText) []
                            let outDomain  = mkDomainIntBTagged (TagEnum nameText)
                                                (fromIntWithTag 1 (TagEnum nameText))
                                                (Reference nameS (Just (Alias (Domain outDomainS))))
                            modify ([(name, outDomain)] `mappend`)
                            return [ Declaration (FindOrGiven Given nameS         outDomainS)
                                   , Declaration (Letting           name  (Domain outDomain))
                                   ]
                        _ -> return [st]

            let

                onD :: Domain () Expression -> Domain () Expression
                onD (DomainEnum nm@(Name nmText) (Just ranges) _)
                    | Just _ <- lookup nm enumDomainNames
                    = DomainInt (TagEnum nmText) ranges
                onD (DomainEnum nm Nothing _)
                    | Just d <- lookup nm enumDomainNames
                    = DomainReference nm (Just d)
                onD (DomainReference nm Nothing)
                    | Just d <- lookup nm enumDomainNames
                    = DomainReference nm (Just d)
                onD p = p


            let model' = model { mStatements = concat statements'
                                    |> transformBi onD
                               }

            logDebug $ "Recording enumGivens:" <+> prettyList id "," (map fst enumDomainNames)

            return model'

        checkEnums model = do
            let
                leftovers :: [Domain () Expression]
                leftovers = [ d | d@DomainEnum{} <- universeBi (mStatements model) ]
            unless (null leftovers) $ bug $ vcat
                $ "Could not remove some enum domains:"
                : map (nest 4 . pretty) leftovers
            return model


removeEnumsFromParam
    :: (MonadFailDoc m, MonadUserError m)
    => Model -> Model -> m (Model, Model)
removeEnumsFromParam model param = do
    let allStatements = map (False,) (map Declaration (miEnumLettings (mInfo model)))
                     ++ map (True,)  (mStatements param)

    (statements', (enumDomainNames_, nameToIntMapping_)) <-
        flip runStateT ([], []) $ forM allStatements $ \ (keep,st) ->
            case st of
                Declaration (LettingDomainDefnEnum ename@(Name enameText) names) -> do
                    namesBefore <- gets (map fst . snd)
                    let outDomain = mkDomainIntBTagged (TagEnum enameText)
                                        (fromIntWithTag 1 (TagEnum enameText))
                                        (fromIntWithTag (genericLength names) (TagEnum enameText))
                    case names `intersect` namesBefore of
                        [] -> modify ( ( [(ename, outDomain)]
                                     , zip names (zip (cycle [ename]) allNats)
                                     ) `mappend` )
                        repeated -> userErr1 $ vcat
                            [ "Some members of this enum domain (" <> pretty ename <> ") seem to be defined"
                            , "as part of other enum domains."
                            , "Repeated:" <+> prettyList id "," repeated
                            , "While working on domain:" <+> pretty st
                            ]
                    return (Just (Declaration (Letting ename (Domain outDomain))))
                _ -> return (if keep then Just st else Nothing)

    let enumDomainNames = M.fromList enumDomainNames_
    let nameToIntMapping = M.fromList nameToIntMapping_

    let
        onX :: Monad m => Expression -> m Expression
        onX (Reference nm Nothing)
            | Just (Name ename, i) <- M.lookup nm nameToIntMapping
            = return (fromIntWithTag i (TagEnum ename))
        onX p = return p

        onC :: Monad m => Constant -> m Constant
        onC (ConstantEnum _ _ nm)
            | Just (Name ename, i) <- M.lookup nm nameToIntMapping
            = return (fromIntWithTag i (TagEnum ename))
        onC p = return p

        onD :: MonadFailDoc m => Domain () Expression -> m (Domain () Expression)
        onD (DomainEnum nm@(Name nmText) (Just ranges) _)
            | Just _ <- M.lookup nm enumDomainNames
            = DomainInt (TagEnum nmText) <$> mapM (mapM (nameToX nameToIntMapping)) ranges
        onD (DomainEnum nm Nothing _)
            | Just d <- M.lookup nm enumDomainNames
            = return (DomainReference nm (Just d))
        onD (DomainReference nm Nothing)
            | Just d <- M.lookup nm enumDomainNames
            = return (DomainReference nm (Just d))
        onD p = return p

    let param' = param { mStatements = catMaybes statements' }
    let f = transformBiM onD >=> transformBiM onX >=> transformBiM onC
    (,) <$> f model <*> f param'


-- | Using the original domains from the Essence file.
--   Converting integers back to enum constants.
-- TODO: complete addEnumsAndUnnamedsBack

addEnumsAndUnnamedsBack
    :: ( Pretty r, Pretty x )
    => [Name]                               -- unnamed types
    -> M.HashMap (Integer, Name) Constant   -- a lookup table for enums
    -> Domain r x                           -- the domain we are working on
    -> Constant                             -- the constant with ints in place of enums & unnameds
    -> Constant                             -- the constant with enums & unnameds again
addEnumsAndUnnamedsBack unnameds ctxt = helper

    where

        helper domain constant = case (domain, constant) of

            (_, TypedConstant c _) -> helper domain c

            (_, c@ConstantUndefined{}) -> c

            (DomainBool  , c) -> c
            (DomainIntE{}, c) -> c
            (DomainInt{} , c) -> c

            (DomainEnum      ename _ _, ConstantInt _ i) ->
                fromMaybe (bug $ "addEnumsAndUnnamedsBack 1:" <+> pretty (i, ename))
                          (M.lookup (i, ename) ctxt)

            (DomainReference ename _  , ConstantInt _ i) ->
                if ename `elem` unnameds
                    then ConstantEnum ename [] (mconcat [ename, "_", Name (T.pack (show i))])
                    else bug $ "addEnumsAndUnnamedsBack Unnamed:" <++> vcat [ "domain  :" <+> pretty domain
                                                                            , "constant:" <+> pretty constant
                                                                            ]

            (DomainTuple ds, viewConstantTuple -> Just cs) ->
                ConstantAbstract $ AbsLitTuple
                    [ helper d c
                    | (d,c) <- zip ds cs ]

            (DomainRecord (sortOn fst -> ds), viewConstantRecord -> Just cs) ->
                ConstantAbstract $ AbsLitRecord
                    [ (n, helper d c)
                    | ((n,d),(_,c)) <- zip ds cs ]

            (DomainVariant ds, viewConstantVariant -> Just (t, n, c)) ->
                case lookup n ds of
                    Nothing -> bug $ "addEnumsAndUnnamedsBack Variant:" <++> vcat [ "domain  :" <+> pretty domain
                                                                                  , "constant:" <+> pretty constant
                                                                                  ]
                    Just d  -> ConstantAbstract $ AbsLitVariant t n (helper d c)

            (DomainMatrix _ inner, viewConstantMatrix -> Just (index, vals)) ->
                ConstantAbstract $ AbsLitMatrix index $ map (helper inner) vals

            (DomainSet _ _ inner, viewConstantSet -> Just vals) ->
                ConstantAbstract $ AbsLitSet $ map (helper inner) vals

            (DomainMSet _ _ inner, viewConstantMSet -> Just vals) ->
                ConstantAbstract $ AbsLitMSet $ map (helper inner) vals

            (DomainFunction _ _ fr to, viewConstantFunction -> Just vals) ->
                ConstantAbstract $ AbsLitFunction
                    [ (helper fr a, helper to b)
                    | (a,b) <- vals ]

            (DomainSequence _ _ inner, viewConstantSequence -> Just vals) ->
                ConstantAbstract $ AbsLitSequence $ map (helper inner) vals

            (DomainRelation _ _ inners, viewConstantRelation -> Just vals) ->
                ConstantAbstract $ AbsLitRelation
                    [ [ helper d c | (d,c) <- zip inners line ]
                    | line <- vals ]

            (DomainPartition _ _ inner, viewConstantPartition -> Just vals) ->
                ConstantAbstract $ AbsLitPartition
                    [ [ helper inner c | c <- line ]
                    | line <- vals ]

            (DomainPermutation _ _ inner, ConstantAbstract (AbsLitPermutation vals)) ->
                ConstantAbstract $ AbsLitPermutation
                   [ [helper inner c | c <- line ]
                   | line <- vals]
            _ -> bug ("addEnumsAndUnnamedsBack 3:" <++> vcat [ "domain  :" <+> pretty domain
                                                             , "constant:" <+> pretty constant
                                                             , "domain  :" <+> pretty (show domain)
                                                             , "constant:" <+> pretty (show constant)
                                                             ])

-- first Name is the value, the second Name is the name of the enum domain
nameToX :: MonadFailDoc m => M.HashMap Name (Name, Integer) -> Expression -> m Expression
nameToX nameToIntMapping (Reference nm _) = case M.lookup nm nameToIntMapping of
    Nothing -> failDoc (pretty nm <+> "is used in a domain, but it isn't a member of the enum domain.")
    Just (Name ename, i)  -> return (fromIntWithTag i (TagEnum ename))
    Just (ename, i) -> bug $ "nameToX, nm:" <+> vcat [pretty (show ename), pretty i]
nameToX _ x = return x
