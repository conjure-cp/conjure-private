{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Conjure.UI.Model
    ( outputModels
    , Strategy(..), Config(..), parseStrategy
    , nbUses
    , modelRepresentationsJSON
    , timedF
    , evaluateModel -- unused, exporting to suppress warning
    , prologue
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Expression.Internal.Generated ()
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.CategoryOf
import Conjure.Language.TypeOf
import Conjure.Compute.DomainOf
import Conjure.Language.DomainSizeOf
import Conjure.Language.Lenses
import Conjure.Language.TH ( essence )
import Conjure.Language.Expression ( reDomExp )
import Conjure.Language.Constant   ( reDomConst )
import Conjure.Language.Expression.Op
import Conjure.Language.ModelStats ( modelInfo )
import Conjure.Language.Instantiate ( instantiateExpression, trySimplify )
import Conjure.Process.Sanity ( sanityChecks )
import Conjure.Process.Enums ( removeEnumsFromModel )
import Conjure.Process.Unnameds ( removeUnnamedsFromModel )
import Conjure.Process.FiniteGivens ( finiteGivens )
import Conjure.Process.LettingsForComplexInDoms ( lettingsForComplexInDoms
                                                , inlineLettingDomainsForDecls
                                                , removeDomainLettings
                                                )
import Conjure.Process.AttributeAsConstraints ( attributeAsConstraints, mkAttributeToConstraint )
import Conjure.Process.InferAttributes ( inferAttributes )
import Conjure.Process.DealWithCuts ( dealWithCuts )
import Conjure.Process.Enumerate ( EnumerateDomain )
import Conjure.Language.NameResolution ( resolveNames, resolveNamesX )
import Conjure.UI.TypeCheck ( typeCheckModel, typeCheckModel_StandAlone )
import Conjure.UI ( OutputFormat(..) )
import Conjure.UI.IO ( writeModel )
import Conjure.UI.NormaliseQuantified ( distinctQuantifiedVars
                                      , renameQuantifiedVarsToAvoidShadowing
                                      , normaliseQuantifiedVariables
                                      , normaliseQuantifiedVariablesS
                                      , normaliseQuantifiedVariablesE
                                      )


import Conjure.Representations
    ( downX, downX1, downD, reprOptions, getStructurals
    , symmetryOrdering
    , reprsStandardOrderNoLevels, reprsStandardOrder, reprsSparseOrder
    )

import Conjure.Rules.Definition

import qualified Conjure.Rules.Vertical.Tuple as Vertical.Tuple
import qualified Conjure.Rules.Vertical.Record as Vertical.Record
import qualified Conjure.Rules.Vertical.Variant as Vertical.Variant
import qualified Conjure.Rules.Vertical.Matrix as Vertical.Matrix

import qualified Conjure.Rules.Horizontal.Set as Horizontal.Set
import qualified Conjure.Rules.Vertical.Set.Explicit as Vertical.Set.Explicit
import qualified Conjure.Rules.Vertical.Set.ExplicitVarSizeWithDummy as Vertical.Set.ExplicitVarSizeWithDummy
import qualified Conjure.Rules.Vertical.Set.ExplicitVarSizeWithFlags as Vertical.Set.ExplicitVarSizeWithFlags
import qualified Conjure.Rules.Vertical.Set.ExplicitVarSizeWithMarker as Vertical.Set.ExplicitVarSizeWithMarker
import qualified Conjure.Rules.Vertical.Set.Occurrence as Vertical.Set.Occurrence

import qualified Conjure.Rules.Horizontal.MSet as Horizontal.MSet
import qualified Conjure.Rules.Vertical.MSet.Occurrence as Vertical.MSet.Occurrence
import qualified Conjure.Rules.Vertical.MSet.ExplicitWithFlags as Vertical.MSet.ExplicitWithFlags
import qualified Conjure.Rules.Vertical.MSet.ExplicitWithRepetition as Vertical.MSet.ExplicitWithRepetition

import qualified Conjure.Rules.Horizontal.Function as Horizontal.Function
import qualified Conjure.Rules.Vertical.Function.Function1D as Vertical.Function.Function1D
import qualified Conjure.Rules.Vertical.Function.Function1DPartial as Vertical.Function.Function1DPartial
import qualified Conjure.Rules.Vertical.Function.FunctionND as Vertical.Function.FunctionND
import qualified Conjure.Rules.Vertical.Function.FunctionNDPartial as Vertical.Function.FunctionNDPartial
import qualified Conjure.Rules.Vertical.Function.FunctionNDPartialDummy as Vertical.Function.FunctionNDPartialDummy
import qualified Conjure.Rules.Vertical.Function.FunctionAsRelation as Vertical.Function.FunctionAsRelation

import qualified Conjure.Rules.Horizontal.Sequence as Horizontal.Sequence
import qualified Conjure.Rules.Vertical.Sequence.ExplicitBounded as Vertical.Sequence.ExplicitBounded

import qualified Conjure.Rules.Horizontal.Relation as Horizontal.Relation
import qualified Conjure.Rules.Vertical.Relation.RelationAsMatrix as Vertical.Relation.RelationAsMatrix
import qualified Conjure.Rules.Vertical.Relation.RelationAsSet as Vertical.Relation.RelationAsSet

import qualified Conjure.Rules.Horizontal.Partition as Horizontal.Partition
import qualified Conjure.Rules.Vertical.Partition.PartitionAsSet as Vertical.Partition.PartitionAsSet
import qualified Conjure.Rules.Vertical.Partition.Occurrence as Vertical.Partition.Occurrence
import qualified Conjure.Rules.Transform as Transform

import qualified Conjure.Rules.Horizontal.Permutation as Horizontal.Permutation
import qualified Conjure.Rules.Vertical.Permutation.PermutationAsFunction as Vertical.Permutation.PermutationAsFunction

import qualified Conjure.Rules.BubbleUp as BubbleUp
import qualified Conjure.Rules.DontCare as DontCare
import qualified Conjure.Rules.TildeOrdering as TildeOrdering

-- base
import System.IO ( hFlush, stdout )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef )
import System.IO.Unsafe ( unsafePerformIO )

-- uniplate
import Data.Generics.Uniplate.Zipper ( hole, replaceHole )
import Data.Generics.Uniplate.Zipper as Zipper ( right, up )

-- pipes
import Pipes ( Pipe, Producer, await, yield, (>->), cat )
import qualified Pipes.Prelude as Pipes ( foldM )

import qualified Data.Aeson.Types as JSON   -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M   -- containers
import qualified Data.Vector as V           -- vector

-- containers
import qualified Data.Set as S

-- text
import qualified Data.Text as T ( stripPrefix )


outputModels ::
    forall m .
    MonadIO m =>
    MonadFailDoc m =>
    MonadLog m =>
    NameGen m =>
    EnumerateDomain m =>
    MonadUserError m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Maybe Int ->                -- portfolioSize
    S.Set Int ->                -- modelHashesBefore
    String ->                   -- modelNamePrefix
    Config ->
    Model ->
    m (S.Set Int)               -- hash values, identifying the models
outputModels portfolioSize modelHashesBefore modelNamePrefix config model = do

    liftIO $ writeIORef recordedResponses (responses config)
    liftIO $ writeIORef recordedResponsesRepresentation (responsesRepresentation config)

    -- Savile Row does not support ' characters in identifiers
    -- We could implement a workaround where we insert a marker (like __PRIME__) for each ' character
    -- and recover these after a solution is found.
    -- But this will be too hairy, instead we will reject such identifiers for now.
    -- If somebody really needs to use a ' character as part of an identifier, we can revisit this decision.
    let
        primeyIdentifiers = catMaybes
            [ if '\'' `elem` textToString identifier
                then Just identifier
                else Nothing
            | Declaration decl <- mStatements model
            , Name identifier <- universeBi decl
            ]
    unless (null primeyIdentifiers) $ userErr1 $ vcat
        ["Identifiers cannot contain a quotation mark character in them:" <+> prettyList id "," primeyIdentifiers]

    let dir = outputDirectory config

    unless (estimateNumberOfModels config) $
        liftIO $ createDirectoryIfMissing True dir

    let
        limitModelsIfEstimating :: Pipe LogOrModel LogOrModel m ()
        limitModelsIfEstimating =
            if estimateNumberOfModels config
                then limitModelsNeeded 1
                else Pipes.cat

        limitModelsIfNeeded :: Pipe LogOrModel LogOrModel m ()
        limitModelsIfNeeded = maybe Pipes.cat limitModelsNeeded (limitModels config)

        limitModelsNeeded :: Int -> Pipe LogOrModel LogOrModel m ()
        limitModelsNeeded 0 = return ()
        limitModelsNeeded n = do
            x <- Pipes.await
            Pipes.yield x
            case x of
                Left {} -> limitModelsNeeded n              -- yielded a log, still n models to produce
                Right{} -> limitModelsNeeded (n-1)          -- yielded a model, produce n-1 more models

        limitModelsPortfolioSize :: Pipe LogOrModel LogOrModel m ()
        limitModelsPortfolioSize =
            case portfolioSize of
                Nothing -> Pipes.cat
                Just s -> do
                    nb <- liftIO (readIORef nbGeneratedModels)
                    if nb < s
                        then do
                            x <- Pipes.await
                            Pipes.yield x
                            limitModelsPortfolioSize
                        else do
                            log LogInfo $ "Stopping, generated" <+> pretty nb <+> "models."
                            return ()

        each (modelHashes, i) logOrModel =
            case logOrModel of
                Left (l,msg) -> do
                    log l msg
                    return (modelHashes, i)
                Right eprime -> do
                    let newHash = eprime { mInfo = def, mStatements = sort (mStatements eprime) }
                                    |> normaliseQuantifiedVariables
                                    |> hash
                    let gen =
                            if modelNamePrefix `elem` ["01_compact", "02_sparse"]
                                then modelNamePrefix
                                else modelNamePrefix ++
                                        if smartFilenames config
                                            then [ choice
                                                 | (_question, choice, numOptions) <-
                                                         eprime |> mInfo |> miTrailCompact
                                                 , numOptions > 1
                                                 ] |> map (('_':) . show)
                                                   |> concat
                                            else padLeft 6 '0' (show i)
                    let filename = dir </> gen ++ ".eprime"
                    if S.member newHash modelHashes
                        then do
                            log LogInfo $ "Skipping duplicate model (" <> pretty filename <> ")"
                            return (modelHashes, i)
                        else do
                            if estimateNumberOfModels config
                                then do
                                    let
                                        estimate :: Integer
                                        estimate = product $ 1 : [ toInteger numOptions
                                                                 | (_question, _choice, numOptions) <-
                                                                     eprime |> mInfo |> miTrailCompact
                                                                 ]
                                    log LogInfo $ "These options would generate at least"
                                                <+> pretty estimate
                                                <+> (if estimate == 1 then "model" else "models") <> "."
                                else do
                                    case portfolioSize of
                                        Nothing -> return ()
                                        Just _ -> log LogInfo $ "Saved model in:" <+> pretty filename
                                    writeModel (lineWidth config) Plain (Just filename) eprime
                                    liftIO $ modifyIORef nbGeneratedModels (+1)
                            let modelHashes' = S.insert newHash modelHashes
                            return (modelHashes', i+1)

    let ?typeCheckerMode = RelaxedIntegerTags

    Pipes.foldM each
                (return (modelHashesBefore, numberingStart config))
                (\ (modelHashes, _nbModels) -> return modelHashes )
                (toCompletion config model
                    >-> limitModelsIfNeeded
                    >-> limitModelsIfEstimating
                    >-> limitModelsPortfolioSize)


toCompletion :: forall m .
    MonadIO m =>
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Config ->
    Model ->
    Producer LogOrModel m ()
toCompletion config m = do
    m2 <- let ?typeCheckerMode = StronglyTyped in prologue config m
    namegenst <- exportNameGenState
    let m2Info = mInfo m2
    let m3 = m2 { mInfo = m2Info { miStrategyQ = strategyQ config
                                 , miStrategyA = strategyA config
                                 , miNameGenState = namegenst
                                 } }
    logDebug $ modelInfo m3
    loopy (StartOver m3)
    where
        driver :: Driver
        driver = strategyToDriver config

        loopy :: ModelWIP -> Producer LogOrModel m ()
        loopy modelWIP = do
            logDebug $ "[loop]" <+> pretty ((modelWIPOut modelWIP) {mInfo = def})
            qs <- remainingWIP config modelWIP
            if null qs
                then do
                    let model = modelWIPOut modelWIP
                    model' <- epilogue model
                    yield (Right model')
                else do
                    nextModels <- driver qs
                    mapM_ loopy nextModels


modelRepresentationsJSON ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    MonadLog m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Config -> Model -> m JSONValue
modelRepresentationsJSON config model = do
    reprs <- modelRepresentations config model
    return $ JSON.Array $ V.fromList
        [ JSON.Object $ KM.fromList
            [ "name" ~~ r name
            , "representations" ~~ representationsJSON
            ]
        | (name, domains) <- reprs
        , let representationsJSON = JSON.Array $ V.fromList
                [ JSON.Object $ KM.fromList
                    [ "description" ~~ r d
                    , "answer" ~~ toJSON i
                    ]
                | (i, d) <- zip allNats domains
                ]
        ]
    where
        (~~) :: JSON.Key -> JSONValue -> (JSON.Key, JSONValue)
        x ~~ y = ( x, y)
        r s = JSON.String $ stringToText $ render 100000 $ pretty s


modelRepresentations ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    MonadLog m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Config -> Model -> m [(Name, [Domain HasRepresentation Expression])]
modelRepresentations config model0 = do
    model <- prologue config model0
    concatForM (mStatements model) $ \case
        Declaration (FindOrGiven _ name domain) -> do
            domOpts <- reprOptions reprsStandardOrderNoLevels domain
            return [(name, domOpts)]
        _ -> return []


-- | If a rule is applied at a position P, the MonadZipper will be retained focused at that location
--   and new rules will be tried using P as the top of the zipper-tree.
--   The whole model (containing P too) will be tried later for completeness.
remainingWIP ::
    MonadFailDoc m =>
    MonadLog m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Config ->
    ModelWIP ->
    m [Question]
remainingWIP config (StartOver model)
    | Just modelZipper <- mkModelZipper model = remaining config modelZipper (mInfo model)
    | otherwise = return []
remainingWIP config wip@(TryThisFirst modelZipper info) = do
    qs <- remaining config modelZipper info
    case (null qs, Zipper.right modelZipper, Zipper.up modelZipper) of
        (False, _, _)  -> return qs                                         -- not null, return
        (_, Just r, _) -> remainingWIP config (TryThisFirst r info)         -- there is a sibling to the right
        (_, _, Just u) -> remainingWIP config (TryThisFirst u info)         -- there is a parent
        _              -> remainingWIP config (StartOver (modelWIPOut wip)) -- we are done here,
                                                                            -- start-over the whole model in case
                                                                            -- something on the left needs attention.


remaining ::
    MonadFailDoc m =>
    MonadLog m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Config ->
    ModelZipper ->
    ModelInfo ->
    m [Question]
remaining config modelZipper minfo = do
    -- note: the call to getQuestions can update the NameGen state
    importNameGenState (minfo |> miNameGenState)
    questions <- getQuestions config modelZipper
    namegenst0 <- exportNameGenState
    forM questions $ \ (focus, answers0) -> do
        answers1 <- forM answers0 $ \ (ruleName, RuleResult{..}) -> do
            importNameGenState namegenst0
            ruleResultExpr <- ruleResult
            -- ruleResultExpr <- fmap fixRelationProj ruleResult   -- TODO: do we need the fixRelationProj?
            let fullModelBeforeHook = replaceHole ruleResultExpr focus
            let mtyBefore = typeOf (hole focus)
            let mtyAfter  = typeOf ruleResultExpr
            case (mtyBefore, mtyAfter) of
                (Right tyBefore, Right tyAfter) ->
                    unless (typesUnify [tyBefore, tyAfter]) $
                        bug $ vcat
                                [ "Rule application changes type:" <+> pretty ruleName
                                , "Before:" <+> pretty (hole focus)
                                , "After :" <+> pretty ruleResultExpr
                                , "Type before:" <+> pretty (show tyBefore)
                                , "Type after :" <+> pretty (show tyAfter)
                                ]
                (Left msg, _) -> bug $ vcat
                                [ "Type error before rule application:" <+> pretty ruleName
                                , "Before:" <+> pretty (hole focus)
                                , "After :" <+> pretty ruleResultExpr
                                , "Error :" <+> pretty msg
                                ]
                (_, Left msg) -> bug $ vcat
                                [ "Type error after rule application:" <+> pretty ruleName
                                , "Before:" <+> pretty (hole focus)
                                , "After :" <+> pretty ruleResultExpr
                                , "Error :" <+> pretty msg
                                ]

            fullModelAfterHook <- case ruleResultHook of
                Nothing   -> do
                    namegenst <- exportNameGenState
                    return (TryThisFirst fullModelBeforeHook minfo { miNameGenState = namegenst })
                Just hook -> do
                    namegenst1 <- exportNameGenState
                    let m1 = fromModelZipper fullModelBeforeHook minfo { miNameGenState = namegenst1 }
                    m2 <- hook m1
                    namegenst2 <- exportNameGenState
                    let m3 = m2 { mInfo = (mInfo m2) { miNameGenState = namegenst2 } }
                    return (StartOver m3)

            return
                ( Answer
                    { aText = ruleName <> ":" <+> ruleResultDescr
                    , aRuleName = ruleName
                    , aBefore = hole focus
                    , aAnswer = ruleResultExpr
                    , aFullModel = fullModelAfterHook
                    }
                , ruleResultType
                )
        let qTypes = map snd answers1
        qType' <- if all (head qTypes ==) (tail qTypes)
                    then return (head qTypes)
                    else bug "Rules of different rule kinds applicable, this is a bug."
        return Question
            { qType = qType'
            , qHole = hole focus
            , qAscendants = tail (ascendants focus)
            , qAnswers = map fst answers1
            }


-- | Computes all applicable questions.
--   strategyQ == PickFirst is special-cased for performance.
getQuestions ::
    MonadLog m =>
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Config ->
    ModelZipper ->
    m [(ModelZipper, [(Doc, RuleResult m)])]
getQuestions config modelZipper | strategyQ config == PickFirst = maybeToList <$>
    let
        loopLevels :: Monad m => [m (Maybe a)] -> m (Maybe a)
        loopLevels [] = return Nothing
        loopLevels (a:as) = do bs <- a
                               case bs of
                                   Nothing -> loopLevels as
                                   Just {} -> return bs

        processLevel :: (MonadFailDoc m, MonadLog m, NameGen m, EnumerateDomain m)
                     => [Rule]
                     -> m (Maybe (ModelZipper, [(Doc, RuleResult m)]))
        processLevel rulesAtLevel =
            let
                go [] = return Nothing
                go (x:xs) = do
                     ys <- applicableRules config rulesAtLevel x
                     if null ys
                         then go xs
                         else return (Just (x, ys))
            in
                go (allContextsExceptReferences modelZipper)
    in
        loopLevels (map processLevel (allRules config))
getQuestions config modelZipper =
    let
        loopLevels :: Monad m => [m [a]] -> m [a]
        loopLevels [] = return []
        loopLevels (a:as) = do bs <- a
                               if null bs
                                   then loopLevels as
                                   else return bs

        processLevel :: (MonadFailDoc m, MonadLog m, NameGen m, EnumerateDomain m)
                     => [Rule]
                     -> m [(ModelZipper, [(Doc, RuleResult m)])]
        processLevel rulesAtLevel =
            fmap catMaybes $ forM (allContextsExceptReferences modelZipper) $ \ x -> do
                ys <- applicableRules config rulesAtLevel x
                return $ if null ys
                            then Nothing
                            else Just (x, ys)
    in
        loopLevels (map processLevel (allRules config))


strategyToDriver :: Config -> Driver
strategyToDriver config questions = do
    let optionsQ =
            [ (doc, q)
            | (n, q) <- zip allNats questions
            , let doc =
                    vcat $ ("Question" <+> pretty n <> ":" <+> pretty (qHole q))
                         : [ nest 4 ("Context #" <> pretty i <> ":" <+> pretty c)
                           | (i,c) <- zip allNats (qAscendants q)
                           -- if logLevel < LogDebugVerbose, only show a select few levels
                           , logLevel config == LogDebugVerbose || i `elem` [1,3,5,10,25]
                           ]
            ]
    pickedQs <- executeStrategy (bug "strategyToDriver no Question") optionsQ (strategyQ config)
    fmap concat $ forM pickedQs $ \ (pickedQNumber, pickedQDescr, pickedQ) -> do
        let optionsA =
                [ (doc, a)
                | (n, a) <- zip allNats (qAnswers pickedQ)
                , let doc = nest 4 $ "Answer" <+> pretty n <> ":" <+>
                        if "choose-repr" `isPrefixOf` show (aRuleName a)
                            then pretty (aText a)
                            else vcat [ pretty (aText a)
                                      , sep [pretty (qHole pickedQ),  "~~>", pretty (aAnswer a)]
                                      ]
                ]
        let strategyA' = case qType pickedQ of
                ChooseRepr            -> representations
                ChooseRepr_Find{}     -> representationsFinds
                ChooseRepr_Given{}    -> representationsGivens
                ChooseRepr_Auxiliary  -> representationsAuxiliaries
                ChooseRepr_Quantified -> representationsQuantifieds
                ChooseRepr_Cut{}      -> representationsCuts
                ExpressionRefinement  -> strategyA
        pickedAs <- executeAnswerStrategy config pickedQ optionsA (strategyA' config)
        return
            [ theModel
            | (pickedANumber, pickedADescr, pickedA) <- pickedAs
            , let upd = addToTrail
                            config
                            (strategyQ  config) pickedQNumber                   pickedQDescr pickedQ
                            (strategyA' config) pickedANumber (length optionsA) pickedADescr pickedA
            , let theModel = updateModelWIPInfo upd (aFullModel pickedA)
            ]


recordedResponses :: IORef (Maybe [Int])
{-# NOINLINE recordedResponses #-}
recordedResponses = unsafePerformIO (newIORef Nothing)

recordedResponsesRepresentation :: IORef (Maybe [(Name, Int)])
{-# NOINLINE recordedResponsesRepresentation #-}
recordedResponsesRepresentation = unsafePerformIO (newIORef Nothing)

nbGeneratedModels :: IORef Int
{-# NOINLINE nbGeneratedModels #-}
nbGeneratedModels = unsafePerformIO (newIORef 0)


executeStrategy :: (MonadIO m, MonadLog m) => Question -> [(Doc, a)] -> Strategy -> m [(Int, Doc, a)]
executeStrategy _ [] _ = bug "executeStrategy: nothing to choose from"
executeStrategy _ [(doc, option)] (viewAuto -> (_, True)) = do
    logDebug ("Picking the only option:" <+> doc)
    return [(1, doc, option)]
executeStrategy question options@((doc, option):_) (viewAuto -> (strategy, _)) =
    case strategy of
        Auto _      -> bug "executeStrategy: Auto"
        PickFirst   -> do
            logDebug ("Picking the first option:" <+> doc)
            return [(1, doc, option)]
        Sparse     -> do
            logDebug ("Picking the first option (in sparse order):" <+> doc)
            return [(1, doc, option)]
        PickAll     -> return [ (i,d,o) | (i,(d,o)) <- zip [1..] options ]
        Interactive -> liftIO $ do
            putStrLn $ render 80 $ vcat (map fst options)
            recordedResponsesRepresentation' <- readIORef recordedResponsesRepresentation
            let
                nextRecordedResponse :: IO (Maybe Int)
                nextRecordedResponse = do
                    mres <- readIORef recordedResponses
                    case mres of
                        Just (next:rest) -> do
                            writeIORef recordedResponses (Just rest)
                            return (Just next)
                        _ -> return Nothing

                nextRecordedResponseRepresentation :: Name -> Maybe Int
                nextRecordedResponseRepresentation nm =
                    case recordedResponsesRepresentation' of
                        Nothing -> Nothing
                        Just mres -> lookup nm mres

                pickIndex :: IO Int
                pickIndex = do
                    let useStoredReprResponse =
                            case qType question of
                                ChooseRepr_Find nm -> Just nm
                                ChooseRepr_Given nm -> Just nm
                                ChooseRepr_Cut nm -> Just nm
                                _ -> Nothing
                    let storedReprResponse =
                            case useStoredReprResponse of
                                Just nm -> nextRecordedResponseRepresentation nm
                                Nothing -> Nothing
                    case storedReprResponse of
                        Just recorded -> do
                            putStrLn ("Response: " ++ show recorded)
                            unless (recorded >= 1 && recorded <= length options) $
                                userErr1 $ vcat [ "Recorded response out of range."
                                                , nest 4 $ "Expected a value between 1 and" <+> pretty (length options)
                                                , nest 4 $ "But got: " <+> pretty recorded
                                                ]
                            return recorded
                        Nothing -> do
                            mrecorded <- nextRecordedResponse
                            case mrecorded of
                                Just recorded -> do
                                    putStrLn ("Response: " ++ show recorded)
                                    unless (recorded >= 1 && recorded <= length options) $
                                        userErr1 $ vcat [ "Recorded response out of range."
                                                        , nest 4 $ "Expected a value between 1 and" <+> pretty (length options)
                                                        , nest 4 $ "But got: " <+> pretty recorded
                                                        ]
                                    return recorded
                                Nothing -> do
                                    putStr "Pick option: "
                                    hFlush stdout
                                    line <- getLine
                                    case (line, readMay line) of
                                        ("", _) -> return 1
                                        (_, Just lineInt) | lineInt >= 1 && lineInt <= length options -> return lineInt
                                        (_, Nothing) -> do
                                            putStrLn "Enter an integer value."
                                            pickIndex
                                        (_, Just _) -> do
                                            print $ pretty $ "Enter a value between 1 and" <+> pretty (length options)
                                            pickIndex

            pickedIndex <- pickIndex
            let (pickedDescr, picked) = at options (pickedIndex - 1)
            return [(pickedIndex, pickedDescr, picked)]
        AtRandom -> do
            let nbOptions = length options
            pickedIndex <- liftIO $ randomRIO (1, nbOptions)
            let (pickedDescr, picked) = at options (pickedIndex - 1)
            logDebug ("Randomly picking option #" <> pretty pickedIndex <+> "out of" <+> pretty nbOptions)
            return [(pickedIndex, pickedDescr, picked)]
        Compact -> bug "executeStrategy: Compact"


executeAnswerStrategy :: (MonadIO m, MonadLog m)
                      => Config -> Question -> [(Doc, Answer)] -> Strategy -> m [(Int, Doc, Answer)]
executeAnswerStrategy _ _ [] _ = bug "executeStrategy: nothing to choose from"
executeAnswerStrategy _ _ [(doc, option)] (viewAuto -> (_, True)) = do
    logDebug ("Picking the only option:" <+> doc)
    return [(1, doc, option)]
executeAnswerStrategy config question options st@(viewAuto -> (strategy, _)) = do
    let
        -- if the trail log does not tell us what to do
        cacheMiss =
            case strategy of
                Compact -> do
                    let (n,(doc,c)) = minimumBy (compactCompareAnswer `on` (snd . snd)) (zip [1..] options)
                    return [(n, doc, c)]
                _  -> executeStrategy question options st

    case M.lookup (hashQuestion question) (followTrail config) of
        Just aHash -> do
            case [ (n, doc, option) | (n, (doc, option)) <- zip [1..] options, hashAnswer option == aHash ] of
                [a] -> do
                    return [a]
                _ -> cacheMiss
        Nothing -> cacheMiss


compactCompareAnswer :: Answer -> Answer -> Ordering
compactCompareAnswer = comparing (expressionDepth . aAnswer)
    where
        expressionDepth :: Data a => a -> Int
        expressionDepth x = 1 + maximum (0 : map expressionDepth (children x))


addToTrail
    :: Config
    -> Strategy -> Int ->        Doc -> Question
    -> Strategy -> Int -> Int -> Doc -> Answer
    -> ModelInfo -> ModelInfo
addToTrail Config{..}
           questionStrategy questionNumber                  questionDescr    theQuestion
           answerStrategy   answerNumber    answerNumbers   answerDescr      theAnswer
           oldInfo = newInfo
    where
        ruleDescr = aText theAnswer
        oldExpr = aBefore theAnswer
        newExpr = aAnswer theAnswer
        newInfo = oldInfo { miTrailCompact      = (questionNumber, answerNumber, answerNumbers)
                                                : miTrailCompact oldInfo
                          , miTrailGeneralised  = (hashQuestion theQuestion, hashAnswer theAnswer)
                                                : miTrailGeneralised oldInfo
                          , miTrailVerbose      = if verboseTrail
                                                      then theA : theQ : miTrailVerbose oldInfo
                                                      else []
                          , miTrailRewrites     = if rewritesTrail
                                                      then theRewrite : miTrailRewrites oldInfo
                                                      else []
                          }
        theQ = Decision
            { dDescription = map (stringToText . renderWide)
                $ ("Question #" <> pretty questionNumber)
                : ("  (Using strategy:" <+> pretty (show questionStrategy) <> ")")
                : map pretty (lines (renderWide questionDescr))
            , dDecision = questionNumber
            , dNumOptions = Nothing
            }
        theA = Decision
            { dDescription = map (stringToText . renderWide)
                $ ("Answer #" <> pretty answerNumber <+> "out of" <+> pretty (show answerNumbers))
                : ("  (Using strategy:" <+> pretty (show answerStrategy) <> ")")
                : map pretty (lines (renderWide answerDescr))
            , dDecision = answerNumber
            , dNumOptions = Just answerNumbers
            }
        theRewrite = TrailRewrites
            { trRule   = stringToText $ renderWide ruleDescr
            , trBefore = map stringToText $ lines $ renderWide $ pretty oldExpr
            , trAfter  = map stringToText $ lines $ renderWide $ pretty newExpr
            }


hashQuestion :: Question -> Int
hashQuestion q = hash (qType q, qHole q, qAscendants q)


hashAnswer :: Answer -> Int
hashAnswer a = hash (aBefore a, renderWide (aRuleName a), aAnswer a)


-- | Add a true-constraint, for every decision variable (whether it is used or not in the model) and
--                          for every parameter (that is not used in the model).
--   A true-constraint has no effect, other than forcing Conjure to produce a representation.
--   It can be used to make sure that a declaration doesn't get lost (if it isn't used anywhere in the model)
--   It can also be used to produce "extra" representations (if it is used in the model)
addTrueConstraints :: Model -> Model
addTrueConstraints m =
    let
        mkTrueConstraint forg nm dom = Op $ MkOpTrue $ OpTrue (Reference nm (Just (DeclNoRepr forg nm dom NoRegion)))
        trueConstraints = [ mkTrueConstraint forg nm d
                          | (Declaration (FindOrGiven forg nm d), after) <- withAfter (mStatements m)
                          , forg == Find || (forg == Given && nbUses nm after == 0)
                          ]
    in
        m { mStatements = mStatements m ++ [SuchThat trueConstraints] }


reverseTrails :: Model -> Model
reverseTrails m =
    let
        oldInfo = mInfo m
        newInfo = oldInfo { miTrailCompact  = reverse (miTrailCompact  oldInfo)
                          , miTrailVerbose  = reverse (miTrailVerbose  oldInfo)
                          , miTrailRewrites = reverse (miTrailRewrites oldInfo)
                          }
    in
        m { mInfo = newInfo }


oneSuchThat :: Model -> Model
oneSuchThat m = m { mStatements = onStatements (mStatements m)
                                    |> nubBy ((==) `on` normaliseQuantifiedVariablesS) }

    where

        onStatements :: [Statement] -> [Statement]
        onStatements xs =
            let
                (suchThats0, objectives, others) = xs |> map collect |> mconcat
                suchThats = suchThats0
                      |> map breakConjunctions                              -- break top level /\'s
                      |> mconcat
                      |> filter (/= Constant (ConstantBool True))           -- remove top level true's
                      |> nubBy ((==) `on` normaliseQuantifiedVariablesE)    -- uniq
            in
                others ++ objectives ++ [SuchThat (combine suchThats)]

        collect :: Statement -> ( [Expression]                  -- SuchThats
                                , [Statement]                   -- Objectives
                                , [Statement]                   -- other statements
                                )
        collect (SuchThat s) = (s, [], [])
        collect s@Objective{} = ([], [s], [])
        collect s = ([], [], [s])

        combine :: [Expression] -> [Expression]
        combine xs = if null xs
                        then [Constant (ConstantBool True)]
                        else xs

        breakConjunctions :: Expression -> [Expression]
        breakConjunctions p@(Op (MkOpAnd (OpAnd x))) =
            case listOut x of
                Nothing -> [p] -- doesn't contain a list
                Just xs -> concatMap breakConjunctions xs
        breakConjunctions x = [x]


emptyMatrixLiterals :: Model -> Model
emptyMatrixLiterals model =
    let
        f (TypeList ty) = TypeMatrix (TypeInt TagInt) ty
        f x = x
    in
        model { mStatements = mStatements model |> transformBi f }


expandDomainReferences :: Model -> Model
expandDomainReferences = transformBi (expandDomainReference :: Domain () Expression -> Domain () Expression)


-- | Add a default search order (branching on [...])
--   to include all the primary variables and none of the aux variables that will potentailly be generated by Conjure.
--   Do not change the model if it already contains a SearchOrder in it.
addSearchOrder :: Model -> Model
addSearchOrder model
    | let hasSearchOrder = not $ null [ () | SearchOrder{} <- mStatements model ]
    , hasSearchOrder = model
    | otherwise =
        let finds = [ nm | Declaration (FindOrGiven Find nm _domain) <- mStatements model ]
        in  model { mStatements = mStatements model ++ [SearchOrder (map BranchingOn finds)] }


inlineDecVarLettings :: Model -> Model
inlineDecVarLettings model =
    let
        inline p@(Reference nm _) = do
            x <- gets (lookup nm)
            return (fromMaybe p x)
        inline p = return p

        statements = catMaybes
                        $ flip evalState []
                        $ forM (mStatements model)
                        $ \ st ->
            case st of
                Declaration (Letting nm x)
                    | categoryOf x == CatDecision
                    -> modify ((nm,x) :) >> return Nothing
                -- The following doesn't work when the identifier is used in a domain
                -- Declaration (Letting nm x@Reference{})
                --     -> modify ((nm,x) :) >> return Nothing
                _ -> Just <$> transformBiM inline st
    in
        model { mStatements = statements }

dropTagForSR ::
    MonadFailDoc m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m Model
dropTagForSR m = do
    let
        replacePredSucc [essence| pred(&x) |] = do
            ty <- typeOf x
            case ty of
                TypeBool{} -> return [essence| false |]
                                -- since True becomes False
                                --       False becomes out-of-bounds, hence False
                TypeInt{}  -> do
                    let xTagInt = reTag TagInt x
                    return [essence| &xTagInt - 1 |]
                _          -> bug "predSucc"
        replacePredSucc [essence| succ(&x) |] = do
            ty <- typeOf x
            case ty of
                TypeBool{} -> return [essence| !&x |]
                                -- since False becomes True
                                --       True becomes out-of-bounds, hence False
                                -- "succ" is exactly "negate" on bools
                TypeInt{}  -> do
                    let xTagInt = reTag TagInt x
                    return [essence| &xTagInt + 1 |]
                _          -> bug "predSucc"
        -- replacePredSucc [essence| &a .< &b |] = return [essence| &a < &b |]
        -- replacePredSucc [essence| &a .<= &b |] = return [essence| &a <= &b |]
        replacePredSucc x = return x

    st <- transformBiM replacePredSucc (mStatements m)
    return m { mStatements = transformBi (\ _ -> TagInt) st }


updateDeclarations ::
    MonadUserError m =>
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m Model
updateDeclarations model = do
    let
        representations = model |> mInfo |> miRepresentations

        onEachStatement (inStatement, afters) =
            case inStatement of
                Declaration (FindOrGiven forg nm _) -> do
                    let
                        -- the refined domains for the high level declaration
                        domains = [ d | (n, d) <- representations, n == nm ]
                    nub <$> concatMapM (onEachDomain forg nm) domains
                Declaration (GivenDomainDefnEnum name) -> return
                    [ Declaration (FindOrGiven Given (name `mappend` "_EnumSize") (DomainInt TagInt [])) ]
                Declaration (Letting nm x)             -> do
                    let
                        usedAfter :: Bool
                        usedAfter = nbUses nm afters > 0

                        nbComplexLiterals :: Int
                        nbComplexLiterals = sum
                                            [ case y of
                                                Constant (ConstantAbstract AbsLitMatrix{}) -> 0
                                                Constant ConstantAbstract{} -> 1
                                                AbstractLiteral AbsLitMatrix{} -> 0
                                                AbstractLiteral{} -> 1
                                                _ -> 0
                                            | y <- universe x ]

                        isRefined :: Bool
                        isRefined = nbComplexLiterals == 0
                    return [inStatement | and [usedAfter, isRefined]]
                Declaration LettingDomainDefnEnum{}    -> return []
                Declaration LettingDomainDefnUnnamed{} -> return []
                SearchOrder orders -> do
                    orders' <- forM orders $ \case
                        BranchingOn nm -> do
                            let domains = [ d | (n, d) <- representations, n == nm ]
                            -- last one is the representation of what's in true(?)
                            -- put that first!
                            let reorder xs =
                                    case reverse xs of
                                        [] -> []
                                        (y:ys) -> y : reverse ys
                            outNames <- concatMapM (onEachDomainSearch nm) (reorder domains)
                            return $ map BranchingOn $ nub outNames
                        Cut{} -> bug "updateDeclarations, Cut shouldn't be here"
                    return [ SearchOrder (concat orders') ]
                _ -> return [inStatement]

        onEachDomain forg nm domain =
            runExceptT (downD (nm, domain)) >>= \case
                Left err -> bug err
                Right outs -> forM outs $ \ (n, d) -> do
                    d' <- transformBiM (trySimplify []) $ forgetRepr d
                    return $ Declaration (FindOrGiven forg n d')

        onEachDomainSearch nm domain =
            runExceptT (downD (nm, domain)) >>= \case
                Left err -> bug err
                Right outs -> return [ n
                                     | (n, _) <- outs
                                     ]

    statements <- concatMapM onEachStatement (withAfter (mStatements model))
    return model { mStatements = statements }


-- | checking whether any `Reference`s with `DeclHasRepr`s are left in the model
checkIfAllRefined :: MonadFailDoc m => Model -> m Model
checkIfAllRefined m | Just modelZipper <- mkModelZipper m = do             -- we exclude the mInfo here
    let returnMsg x = return
            $ ""
            : ("Not refined:" <+> pretty (hole x))
                              <+> stringToDoc (show (hole x))
            : [ nest 4 ("Context #" <> pretty i <> ":" <+> pretty c)
              | (i, c) <- zip allNats (tail (ascendants x))
              ]

    fails <- fmap (nubBy (\a b->show a == show b) . concat) $ forM (allContextsExceptReferences modelZipper) $ \ x ->
                case hole x of
                    Reference _ (Just (DeclHasRepr _ _ dom))
                        | not (isPrimitiveDomain dom) ->
                        return $ ""
                               : ("Not refined:" <+> pretty (hole x))
                                                 <+> stringToDoc (show (hole x))
                               : ("Domain     :" <+> pretty dom)
                               : [ nest 4 ("Context #" <> pretty i <> ":" <+> pretty c)
                                 | (i, c) <- zip allNats (tail (ascendants x))
                                 ]
                    Constant (ConstantAbstract AbsLitMatrix{}) -> return []
                    Constant ConstantAbstract{} -> returnMsg x
                    AbstractLiteral AbsLitMatrix{} -> return []
                    AbstractLiteral{} -> returnMsg x
                    WithLocals{} -> returnMsg x
                    Comprehension _ stmts -> do
                        decisionConditions <-
                            fmap catMaybes $ forM stmts $ \ stmt -> case stmt of
                                Condition c ->
                                    if categoryOf c >= CatDecision
                                        then return (Just c)
                                        else return Nothing
                                _ -> return Nothing
                        comprehensionLettings <-
                            fmap catMaybes $ forM stmts $ \ stmt -> case stmt of
                                ComprehensionLetting{} -> return (Just stmt)
                                _ -> return Nothing
                        unsupportedGenerator <-
                            fmap catMaybes $ forM stmts $ \ stmt -> case stmt of
                                Generator GenInExpr{} -> return (Just stmt)
                                _ -> return Nothing
                        let msgs =  [ "decision expressions as conditions"
                                    | not (null decisionConditions) ]
                                 ++ [ "local lettings"
                                    | not (null comprehensionLettings) ]
                                 ++ [ "unsupported generators"
                                    | not (null unsupportedGenerator) ]
                        let msg = "Comprehension contains" <+> prettyListDoc id "," msgs <> "."
                        case msgs of
                            [] -> return []
                            _  -> return $ [ msg ]
                                        ++ [ nest 4 (pretty (hole x)) ]
                                        ++ [ nest 4 ("Context #" <> pretty i <> ":" <+> pretty c)
                                           | (i, c) <- zip allNats (tail (ascendants x))
                                           ]
                    [essence| &_ .< &_ |] ->
                        return ["", "Not refined:" <+> vcat [ pretty (hole x)
                                                            , stringToDoc (show (hole x))
                                                            ]]
                    [essence| &_ .<= &_ |] ->
                        return ["", "Not refined:" <+> vcat [ pretty (hole x)
                                                            , stringToDoc (show (hole x))
                                                            ]]
                    _ -> return []
    unless (null fails) (bug (vcat fails))
    return m
checkIfAllRefined m = return m


-- | checking whether any undefined values creeped into the final model
checkIfHasUndefined :: MonadFailDoc m => Model -> m Model
checkIfHasUndefined m  | Just modelZipper <- mkModelZipper m = do
    let returnMsg x = return
            $ ""
            : ("Undefined value in the final model:" <+> pretty (hole x))
            : [ nest 4 ("Context #" <> pretty i <> ":" <+> pretty c)
              | (i, c) <- zip allNats (tail (ascendants x))
              ]

    fails <- fmap concat $ forM (allContextsExceptReferences modelZipper) $ \ x ->
                case hole x of
                    Constant ConstantUndefined{} -> returnMsg x
                    _ -> return []
    unless (null fails) (bug (vcat fails))
    return m
checkIfHasUndefined m = return m


topLevelBubbles ::
    MonadFailDoc m =>
    MonadUserError m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m Model
topLevelBubbles m = do
    let
        onStmt (SuchThat xs) = onExprs xs
        onStmt (Where    xs) = concatMapM onWheres xs
        onStmt (Objective obj (WithLocals h locals)) =
            case locals of
                AuxiliaryVars          locs -> (           locs  ++ [Objective obj h] ) |> onStmts
                DefinednessConstraints locs -> ( [SuchThat locs] ++ [Objective obj h] ) |> onStmts
        onStmt (Declaration decl) =
            let
                f (WithLocals h locs) = tell [locs] >> return h
                f x = return x

                (decl', locals) = runWriter (transformBiM f decl)

                conv :: InBubble -> [Statement]
                conv (AuxiliaryVars locs) = locs
                conv (DefinednessConstraints locs) = [SuchThat locs]

                newStmts :: [Statement]
                newStmts = concatMap conv locals
            in
                if null newStmts
                    then return [Declaration decl]
                    else onStmts (newStmts ++ [Declaration decl'])
        onStmt s = return [s]

        -- a where that has a bubble at the top-most level will be replaced
        -- with a Comprehension. this is to avoid creating a where with decision variables inside.
        onWheres (WithLocals h (DefinednessConstraints locals)) =
            return $ map (Where . return) (locals ++ [h])
        onWheres (WithLocals h (AuxiliaryVars locals)) = do
            let (localfinds, gens) = mconcat
                    [ case local of
                        Declaration (FindOrGiven LocalFind nm dom) ->
                            ([nm], [Generator (GenDomainNoRepr (Single nm) dom)])
                        SuchThat xs ->
                            ([], map Condition xs)
                        _ -> bug ("topLevelBubbles.onWheres:" <+> pretty local)
                    | local <- locals
                    ]
            let forgetReprsOfLocalFinds (Reference nm _) | nm `elem` localfinds = Reference nm Nothing
                forgetReprsOfLocalFinds x = descend forgetReprsOfLocalFinds x
            let out = Comprehension h gens
            out' <- resolveNamesX (forgetReprsOfLocalFinds out)
            return [Where [out']]
        onWheres x = return [Where [x]]

        onExpr (WithLocals h (AuxiliaryVars          locals)) = (          locals  ++ [SuchThat [h]]) |> onStmts
        onExpr (WithLocals h (DefinednessConstraints locals)) = ([SuchThat locals] ++ [SuchThat [h]]) |> onStmts
        onExpr x = return [SuchThat [x]]

        onStmts = concatMapM onStmt
        onExprs = concatMapM onExpr

    statements' <- onStmts (mStatements m)
    return m { mStatements = statements' }


sliceThemMatrices ::
    Monad m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m Model
sliceThemMatrices model = do
    let
        -- nothing stays with a matrix type
        -- we are doing this top down
        -- when we reach a matrix-typed expression, we know it needs to be sliced
        -- we descend otherwise
        -- we also descend into components of the matrix-typed expression during slicing
        onExpr :: Monad m => Expression -> m Expression
        onExpr p = do
            let computeExistingSlices t =
                    case match opSlicing t of
                        Nothing -> return 0
                        Just (t', _, _) -> (+1) <$> computeExistingSlices t'
            let isIndexedMatrix = do
                    (m, is) <- match opMatrixIndexing p
                    tyM     <- typeOf m
                    nSlices <- computeExistingSlices m
                    return (m, nSlices, is, tyM)
            case isIndexedMatrix of
                Nothing -> descendM onExpr p
                Just (m, existingSlices, is, tyM) -> do
                    let nestingLevel (TypeMatrix _ a) = 1 + nestingLevel a
                        nestingLevel (TypeList     a) = 1 + nestingLevel a
                        nestingLevel _ = 0 :: Int
                    -- "is" is the number of existing indices
                    -- "nestingLevel" is the nesting level of the original matrix
                    -- "existingSlices" is the number of existing slices
                    let howMany = nestingLevel tyM - existingSlices - length is
                    let unroll a 0 = a
                        unroll a i = make opSlicing (unroll a (i-1)) Nothing Nothing
                    m'  <- descendM onExpr m
                    is' <- mapM onExpr is
                    let p' = make opMatrixIndexing m' is'
                    return $ unroll p' howMany

    statements <- descendBiM onExpr (mStatements model)
    return model { mStatements = statements }


removeExtraSlices :: Monad m => Model -> m Model
removeExtraSlices model = do
    let
        -- a slice at the end of a chain of slices & indexings
        -- does no good in Essence and should be removed
        onExpr :: Monad m => Expression -> m Expression
        onExpr (match opSlicing -> Just (m,_,_)) = onExpr m
        onExpr p@(match opIndexing -> Just _) = return p
        onExpr p = descendM onExpr p

    statements <- descendBiM onExpr (mStatements model)
    return model { mStatements = statements }


removeUnderscores :: Monad m => Model -> m Model
removeUnderscores model = do
    let
        -- SR doesn't support identifiers that start with _
        -- we replace them with UNDERSCORE_
        onName :: Name -> Name
        onName (Name t) =
            case T.stripPrefix "_" t of
                Nothing -> Name t
                Just t' -> Name (mappend "UNDERSCORE__" t')
        onName n = n

    return $ transformBi onName model


lexSingletons :: (?typeCheckerMode :: TypeCheckerMode)
              => Monad m
              => Model -> m Model
lexSingletons model = do
  let onExpr ::  (?typeCheckerMode :: TypeCheckerMode)
              => Monad m => Expression -> m Expression
      onExpr [essence| &l <lex &r |] =
        case (matchSingleton l, matchSingleton r) of
          (Nothing, Nothing) -> return [essence| &l <lex &r |]
          (Just ls, Just rs) -> return [essence| &ls < &rs |]
          _ -> bug $ "lexSingleton: match inconsistent"
      onExpr [essence| &l <=lex &r |] =
        case (matchSingleton l, matchSingleton r) of
          (Nothing, Nothing) -> return [essence| &l <=lex &r |]
          (Just ls, Just rs) -> return [essence| &ls <= &rs |]
          _ -> bug $ "lexSingleton: match inconsistent"
      onExpr x = return x
      matchSingleton ::  (?typeCheckerMode :: TypeCheckerMode)
              => Expression -> Maybe Expression
      matchSingleton (match matrixLiteral -> Just (TypeMatrix _ TypeInt{},_,[s])) =
        Just s
      matchSingleton _ = Nothing
  statements <- transformBiM onExpr (mStatements model)
  return model { mStatements = statements }


logDebugIdModel :: MonadLog m => Doc -> Model -> m Model
logDebugIdModel msg a = logDebug (msg <++> pretty (a {mInfo = def})) >> return a

prologue ::
    MonadFailDoc m =>
    MonadLog m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Config ->
    Model ->
    m Model
prologue config model = do
    void $ typeCheckModel_StandAlone model
    return model                      >>= logDebugIdModel "[input]"
    >>= enforceTagConsistency         >>= logDebugIdModel "[enforceTagConsistency]"
    >>= removeUnderscores             >>= logDebugIdModel "[removeUnderscores]"
    >>= return . addSearchOrder       >>= logDebugIdModel "[addSearchOrder]"
    >>= attributeAsConstraints        >>= logDebugIdModel "[attributeAsConstraints]"
    >>= inferAttributes               >>= logDebugIdModel "[inferAttributes]"
    >>= inlineLettingDomainsForDecls  >>= logDebugIdModel "[inlineLettingDomainsForDecls]"
    >>= lettingsForComplexInDoms      >>= logDebugIdModel "[lettingsForComplexInDoms]"
    >>= distinctQuantifiedVars        >>= logDebugIdModel "[distinctQuantifiedVars]"
    >>= return . initInfo             >>= logDebugIdModel "[initInfo]"
    >>= addUnnamedSymmetryBreaking (unnamedSymmetryBreaking config)
                                      >>= logDebugIdModel "[addUnnamedSymmetryBreaking]"
    >>= removeUnnamedsFromModel       >>= logDebugIdModel "[removeUnnamedsFromModel]"
    >>= removeEnumsFromModel          >>= logDebugIdModel "[removeEnumsFromModel]"
    >>= finiteGivens                  >>= logDebugIdModel "[finiteGivens]"
    >>= renameQuantifiedVarsToAvoidShadowing
                                      >>= logDebugIdModel "[renameQuantifiedVarsToAvoidShadowing]"
    >>= resolveNames                  >>= logDebugIdModel "[resolveNames]"
    >>= return . initInfo_Lettings    >>= logDebugIdModel "[initInfo_Lettings]"
    >>= removeDomainLettings          >>= logDebugIdModel "[removeDomainLettings]"
    >>= (let ?typeCheckerMode = RelaxedIntegerTags in typeCheckModel)
                                      >>= logDebugIdModel "[typeCheckModel]"
    >>= categoryChecking              >>= logDebugIdModel "[categoryChecking]"
    >>= sanityChecks                  >>= logDebugIdModel "[sanityChecks]"
    >>= dealWithCuts                  >>= logDebugIdModel "[dealWithCuts]"
    >>= removeExtraSlices             >>= logDebugIdModel "[removeExtraSlices]"
    -- >>= evaluateModel                 >>= logDebugIdModel "[evaluateModel]"
    >>= return . addTrueConstraints   >>= logDebugIdModel "[addTrueConstraints]"
    >>= enforceTagConsistency         >>= logDebugIdModel "[enforceTagConsistency]"


epilogue ::
    MonadFailDoc m =>
    MonadLog m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m Model
epilogue model = return model
                                      >>= logDebugIdModel "[epilogue]"
    >>= lexSingletons                 >>= logDebugIdModel "[lexSingletons]"
    >>= resolveNames                  >>= logDebugIdModel "[resolveNames]"
    >>= updateDeclarations            >>= logDebugIdModel "[updateDeclarations]"
    >>= return . inlineDecVarLettings >>= logDebugIdModel "[inlineDecVarLettings]"
    >>= topLevelBubbles               >>= logDebugIdModel "[topLevelBubbles]"
    >>= checkIfAllRefined             >>= logDebugIdModel "[checkIfAllRefined]"
    >>= checkIfHasUndefined           >>= logDebugIdModel "[checkIfHasUndefined]"
    >>= sliceThemMatrices             >>= logDebugIdModel "[sliceThemMatrices]"
    >>= dropTagForSR                  >>= logDebugIdModel "[dropTagForSR]"
    >>= return . emptyMatrixLiterals  >>= logDebugIdModel "[emptyMatrixLiterals]"
    >>= return . expandDomainReferences
                                      >>= logDebugIdModel "[expandDomainReferences]"
    >>= return . reverseTrails        >>= logDebugIdModel "[reverseTrails]"
    >>= return . oneSuchThat          >>= logDebugIdModel "[oneSuchThat]"
    >>= return . languageEprime       >>= logDebugIdModel "[languageEprime]"


applicableRules :: forall m n .
    MonadUserError n =>
    MonadFailDoc n =>
    MonadLog n =>
    NameGen n =>
    EnumerateDomain n =>
    MonadUserError m =>
    MonadLog m =>
    NameGen m =>
    EnumerateDomain m =>
    MonadFailDoc m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Config ->
    [Rule] ->
    ModelZipper ->
    n [(Doc, RuleResult m)]
applicableRules Config{..} rulesAtLevel x = do
    let logAttempt = if logRuleAttempts  then logInfo else const (return ())
    let logFail    = if logRuleFails     then logInfo else const (return ())
    let logSuccess = if logRuleSuccesses then logInfo else const (return ())

    mys <- sequence [ do logAttempt ("attempting rule" <+> rName r <+> "on" <+> pretty (hole x))
                         applied <- runExceptT $ runReaderT (rApply r x (hole x)) x
                         return (rName r, applied)
                    | r <- rulesAtLevel ]
    forM_ mys $ \ (rule, my) ->
        case my of
            Left  failed -> unless ("N/A" `isPrefixOf` show failed) $ logFail $ vcat
                [ " rule failed:" <+> rule
                , "          on:" <+> pretty (hole x)
                , "     message:" <+> failed
                ]
            Right _ -> return ()
    return [ (name, res {ruleResult = ruleResult'})
           | (name, Right ress) <- mys
           , res <- ress
           , let ruleResult' = do
                    rResult <- ruleResult res
                    case (hole x, rResult) of
                        (Reference nm1 _, Reference nm2 _)
                            | show name /= "choose-repr"
                            , nm1 == nm2 -> bug $ vcat
                            [ "Rule applied inside a Reference."
                            , "Rule              :" <+> pretty name
                            , "Rule input        :" <+> pretty (hole x)
                            , "Rule output       :" <+> pretty rResult
                            , "Rule input  (show):" <+> pretty (show (hole x))
                            , "Rule output (show):" <+> pretty (show rResult)
                            ]
                        _ -> return ()
                    merr <- runExceptT (resolveNamesX rResult)
                    case merr of
                        Left err -> bug $ vcat
                            [ "Name resolution failed after rule application."
                            , "Rule              :" <+> pretty name
                            , "Rule input        :" <+> pretty (hole x)
                            , "Rule output       :" <+> pretty rResult
                            , "Rule input  (show):" <+> pretty (show (hole x))
                            , "Rule output (show):" <+> pretty (show rResult)
                            , "The error         :" <+> err
                            ]
                        Right r  -> do
                            logSuccess $ vcat
                                [ "rule applied:" <+> name
                                , "          on:" <+> pretty (hole x)
                                , "      output:" <+> pretty r
                                ]
                            return r
           ]


allRules :: (?typeCheckerMode :: TypeCheckerMode) => Config -> [[Rule]]
allRules config =
    [ Transform.rules_Transform
    , [ rule_FullEvaluate
      ]
    , [ rule_PartialEvaluate
      ]
    ] ++ paramRules ++
    [ [ rule_ChooseRepr                 config
      , rule_ChooseReprForComprehension config
      , rule_ChooseReprForLocals        config
      ]
    ] ++ bubbleUpRules ++
    [ [ rule_Eq
      , rule_Neq
      , rule_Comprehension_Cardinality
      , rule_Flatten_Cardinality
      ]
    , verticalRules
    , horizontalRules
    ] ++ otherRules
      ++ delayedRules


-- | For information that can be readily pulled out from parameters.
--   Some things are easier when everything involved is a param.
--   These rules aren't necessary for correctness, but they can help remove some verbose expressions from the output.
--   Make Savile Row happier so it makes us happier. :)
paramRules :: [[Rule]]
paramRules =
    [ [ Horizontal.Set.rule_Param_MinOfSet
      , Horizontal.Set.rule_Param_MaxOfSet
      , Horizontal.Set.rule_Param_Card
      ]
    , [ Horizontal.Function.rule_Param_DefinedRange
      , Horizontal.Relation.rule_Param_Card
      ]
    ]

verticalRules :: [Rule]
verticalRules =
    [ Vertical.Permutation.PermutationAsFunction.rule_Image
    , Vertical.Permutation.PermutationAsFunction.rule_Image_permInverse
    , Vertical.Permutation.PermutationAsFunction.rule_Cardinality
    , Vertical.Permutation.PermutationAsFunction.rule_Defined
    , Vertical.Permutation.PermutationAsFunction.rule_Comprehension


    , Vertical.Tuple.rule_Tuple_Eq
    , Vertical.Tuple.rule_Tuple_Neq
    , Vertical.Tuple.rule_Tuple_Leq
    , Vertical.Tuple.rule_Tuple_Lt
    , Vertical.Tuple.rule_Tuple_TildeLeq
    , Vertical.Tuple.rule_Tuple_TildeLt
    , Vertical.Tuple.rule_Tuple_DotLeq
    , Vertical.Tuple.rule_Tuple_Index



    , Vertical.Record.rule_Record_Eq
    , Vertical.Record.rule_Record_Neq
    , Vertical.Record.rule_Record_Leq
    , Vertical.Record.rule_Record_Lt
    , Vertical.Record.rule_Record_Index

    , Vertical.Variant.rule_Variant_Eq
    , Vertical.Variant.rule_Variant_Neq
    , Vertical.Variant.rule_Variant_Leq
    , Vertical.Variant.rule_Variant_Lt
    , Vertical.Variant.rule_Variant_Index
    , Vertical.Variant.rule_Variant_Active

    , Vertical.Matrix.rule_Comprehension_Literal
    , Vertical.Matrix.rule_Comprehension
    , Vertical.Matrix.rule_Comprehension_Flatten
    , Vertical.Matrix.rule_ModifierAroundIndexedMatrixLiteral
    , Vertical.Matrix.rule_Comprehension_LiteralIndexed
    , Vertical.Matrix.rule_Comprehension_Nested
    , Vertical.Matrix.rule_Comprehension_Hist
    , Vertical.Matrix.rule_Comprehension_ToSet_Matrix
    , Vertical.Matrix.rule_Comprehension_ToSet_List
    , Vertical.Matrix.rule_Comprehension_ToSet_List_DuplicateFree
    , Vertical.Matrix.rule_Matrix_Eq
    , Vertical.Matrix.rule_Matrix_Neq
    , Vertical.Matrix.rule_Matrix_Leq_Primitive
    , Vertical.Matrix.rule_Matrix_Leq_Decompose
    , Vertical.Matrix.rule_Matrix_Lt_Primitive
    , Vertical.Matrix.rule_Matrix_Lt_Decompose
    , Vertical.Matrix.rule_IndexingIdentical
    , Vertical.Matrix.rule_ExpandSlices
    , Vertical.Matrix.rule_Freq

    , Vertical.Set.Explicit.rule_Min
    , Vertical.Set.Explicit.rule_Max
    , Vertical.Set.Explicit.rule_Card
    , Vertical.Set.Explicit.rule_Comprehension
    , Vertical.Set.Explicit.rule_PowerSet_Comprehension
    , Vertical.Set.Explicit.rule_In
    , Vertical.Set.ExplicitVarSizeWithDummy.rule_Comprehension
    , Vertical.Set.ExplicitVarSizeWithDummy.rule_PowerSet_Comprehension
    , Vertical.Set.ExplicitVarSizeWithFlags.rule_Comprehension
    , Vertical.Set.ExplicitVarSizeWithFlags.rule_PowerSet_Comprehension
    , Vertical.Set.ExplicitVarSizeWithMarker.rule_Card
    , Vertical.Set.ExplicitVarSizeWithMarker.rule_Comprehension
    , Vertical.Set.ExplicitVarSizeWithMarker.rule_PowerSet_Comprehension
    , Vertical.Set.Occurrence.rule_Comprehension
    , Vertical.Set.Occurrence.rule_PowerSet_Comprehension
    , Vertical.Set.Occurrence.rule_In

    , Vertical.MSet.Occurrence.rule_Comprehension
    , Vertical.MSet.Occurrence.rule_Freq

    , Vertical.MSet.ExplicitWithFlags.rule_Comprehension
    , Vertical.MSet.ExplicitWithFlags.rule_Freq

    , Vertical.MSet.ExplicitWithRepetition.rule_Comprehension

    , Vertical.Function.Function1D.rule_Comprehension
    , Vertical.Function.Function1D.rule_Comprehension_Defined
    , Vertical.Function.Function1D.rule_Image

    , Vertical.Function.Function1DPartial.rule_Comprehension
    , Vertical.Function.Function1DPartial.rule_PowerSet_Comprehension
    , Vertical.Function.Function1DPartial.rule_Image_NotABool
    , Vertical.Function.Function1DPartial.rule_Image_Bool
    , Vertical.Function.Function1DPartial.rule_InDefined
    , Vertical.Function.Function1DPartial.rule_DefinedEqDefined

    , Vertical.Function.FunctionND.rule_Comprehension
    , Vertical.Function.FunctionND.rule_Comprehension_Defined
    , Vertical.Function.FunctionND.rule_Image

    , Vertical.Function.FunctionNDPartial.rule_Comprehension
    , Vertical.Function.FunctionNDPartial.rule_Image_NotABool
    , Vertical.Function.FunctionNDPartial.rule_Image_Bool
    , Vertical.Function.FunctionNDPartial.rule_InDefined

    , Vertical.Function.FunctionNDPartialDummy.rule_Comprehension
    , Vertical.Function.FunctionNDPartialDummy.rule_Image
    , Vertical.Function.FunctionNDPartialDummy.rule_InDefined

    , Vertical.Function.FunctionAsRelation.rule_Comprehension
    -- , Vertical.Function.FunctionAsRelation.rule_PowerSet_Comprehension
    , Vertical.Function.FunctionAsRelation.rule_Image_Eq
    , Vertical.Function.FunctionAsRelation.rule_InDefined
    , Vertical.Function.FunctionAsRelation.rule_InToSet

    , Vertical.Sequence.ExplicitBounded.rule_Comprehension
    , Vertical.Sequence.ExplicitBounded.rule_Card
    , Vertical.Sequence.ExplicitBounded.rule_Image_Bool
    , Vertical.Sequence.ExplicitBounded.rule_Image_NotABool
    , Vertical.Sequence.ExplicitBounded.rule_Leq
    , Vertical.Sequence.ExplicitBounded.rule_Lt

    , Vertical.Relation.RelationAsMatrix.rule_Comprehension
    , Vertical.Relation.RelationAsMatrix.rule_Image

    , Vertical.Relation.RelationAsSet.rule_Comprehension
    , Vertical.Relation.RelationAsSet.rule_PowerSet_Comprehension
    , Vertical.Relation.RelationAsSet.rule_Card
    , Vertical.Relation.RelationAsSet.rule_In

    , Vertical.Partition.PartitionAsSet.rule_Comprehension
    , Vertical.Partition.Occurrence.rule_Comprehension

    ]

horizontalRules :: [Rule]
horizontalRules =
    [ Horizontal.Permutation.rule_Cardinality_Literal
    , Horizontal.Permutation.rule_Equality
    , Horizontal.Permutation.rule_Disequality
    , Horizontal.Permutation.rule_Comprehension
    , Horizontal.Permutation.rule_Compose_Image




    , Horizontal.Permutation.rule_Defined_Literal
    , Horizontal.Permutation.rule_Image_Literal
    , Horizontal.Permutation.rule_In
    , Horizontal.Permutation.rule_Permutation_Inverse





    , Horizontal.Set.rule_Comprehension_Literal
    , Horizontal.Set.rule_Eq
    , Horizontal.Set.rule_Neq
    , Horizontal.Set.rule_Subset
    , Horizontal.Set.rule_SubsetEq
    , Horizontal.Set.rule_Supset
    , Horizontal.Set.rule_SupsetEq
    , Horizontal.Set.rule_In
    , Horizontal.Set.rule_Card
    , Horizontal.Set.rule_CardViaFreq
    , Horizontal.Set.rule_Intersect
    , Horizontal.Set.rule_Union
    , Horizontal.Set.rule_Difference
    , Horizontal.Set.rule_PowerSet_Comprehension
    , Horizontal.Set.rule_PowerSet_Difference
    , Horizontal.Set.rule_MaxMin

    , Horizontal.MSet.rule_Comprehension_Literal
    , Horizontal.MSet.rule_Comprehension_ToSet_Literal
    , Horizontal.MSet.rule_Eq
    , Horizontal.MSet.rule_Neq
    , Horizontal.MSet.rule_Subset
    , Horizontal.MSet.rule_SubsetEq
    , Horizontal.MSet.rule_Supset
    , Horizontal.MSet.rule_SupsetEq
    , Horizontal.MSet.rule_Freq
    , Horizontal.MSet.rule_In
    , Horizontal.MSet.rule_Card
    , Horizontal.MSet.rule_MaxMin

    , Horizontal.Function.rule_Comprehension_Literal
    , Horizontal.Function.rule_Image_Bool
    , Horizontal.Function.rule_Image_BoolMatrixIndexed
    , Horizontal.Function.rule_Image_BoolTupleIndexed
    , Horizontal.Function.rule_Image_Int
    , Horizontal.Function.rule_Image_IntMatrixIndexed
    , Horizontal.Function.rule_Image_IntTupleIndexed
    , Horizontal.Function.rule_Image_Matrix_LexLhs
    , Horizontal.Function.rule_Image_Matrix_LexRhs

    , Horizontal.Function.rule_Comprehension_Image
    , Horizontal.Function.rule_Comprehension_ImageSet
    , Horizontal.Function.rule_Eq
    , Horizontal.Function.rule_Neq
    , Horizontal.Function.rule_Subset
    , Horizontal.Function.rule_SubsetEq
    , Horizontal.Function.rule_Supset
    , Horizontal.Function.rule_SupsetEq
    , Horizontal.Function.rule_Inverse
    , Horizontal.Function.rule_Card
    , Horizontal.Function.rule_Comprehension_PreImage
    , Horizontal.Function.rule_Comprehension_Defined
    , Horizontal.Function.rule_Comprehension_Range
    , Horizontal.Function.rule_In
    , Horizontal.Function.rule_Restrict_Image
    , Horizontal.Function.rule_Restrict_Comprehension
    , Horizontal.Function.rule_Comprehension_Defined_Size
    , Horizontal.Function.rule_Comprehension_Range_Size
    , Horizontal.Function.rule_Defined_Intersect
    , Horizontal.Function.rule_DefinedOrRange_Union
    , Horizontal.Function.rule_DefinedOrRange_Difference

    , Horizontal.Sequence.rule_Comprehension_Literal
    , Horizontal.Sequence.rule_Image_Bool
    , Horizontal.Sequence.rule_Image_Int
    , Horizontal.Sequence.rule_Comprehension_Image
    , Horizontal.Sequence.rule_Image_Literal_Bool
    , Horizontal.Sequence.rule_Image_Literal_Int
    , Horizontal.Sequence.rule_Eq_Literal
    , Horizontal.Sequence.rule_Eq
    , Horizontal.Sequence.rule_Eq_Comprehension
    , Horizontal.Sequence.rule_Neq
    , Horizontal.Sequence.rule_Subset
    , Horizontal.Sequence.rule_SubsetEq
    , Horizontal.Sequence.rule_Supset
    , Horizontal.Sequence.rule_SupsetEq
    , Horizontal.Sequence.rule_Card
    , Horizontal.Sequence.rule_Comprehension_PreImage
    , Horizontal.Sequence.rule_Comprehension_Defined
    , Horizontal.Sequence.rule_Comprehension_Range
    , Horizontal.Sequence.rule_In
    , Horizontal.Sequence.rule_Restrict_Image
    , Horizontal.Sequence.rule_Restrict_Comprehension
    , Horizontal.Sequence.rule_Substring
    , Horizontal.Sequence.rule_Subsequence

    , Horizontal.Relation.rule_Comprehension_Literal
    , Horizontal.Relation.rule_Comprehension_Projection
    , Horizontal.Relation.rule_PowerSet_Comprehension
    , Horizontal.Relation.rule_Image
    , Horizontal.Relation.rule_In
    , Horizontal.Relation.rule_Eq
    , Horizontal.Relation.rule_Neq
    , Horizontal.Relation.rule_Subset
    , Horizontal.Relation.rule_SubsetEq
    , Horizontal.Relation.rule_Supset
    , Horizontal.Relation.rule_SupsetEq
    , Horizontal.Relation.rule_Card

    , Horizontal.Partition.rule_Comprehension_Literal
    , Horizontal.Partition.rule_Eq
    , Horizontal.Partition.rule_Neq
    , Horizontal.Partition.rule_Together
    , Horizontal.Partition.rule_Apart
    , Horizontal.Partition.rule_Party
    , Horizontal.Partition.rule_Participants
    , Horizontal.Partition.rule_Card
    , Horizontal.Partition.rule_In


    ]


bubbleUpRules :: [[Rule]]
bubbleUpRules =
    [
        [ BubbleUp.rule_MergeNested
        , BubbleUp.rule_ToAnd
        , BubbleUp.rule_ToMultiply_HeadOfIntComprehension
        , BubbleUp.rule_ConditionInsideGeneratorDomain
        , BubbleUp.rule_LiftVars
        ]
    ,
        [ BubbleUp.rule_NotBoolYet
        ]
    ]


otherRules :: [[Rule]]
otherRules =
    [
        [ rule_Xor_To_Sum ]
    ,
        [ TildeOrdering.rule_BoolInt
        , TildeOrdering.rule_MSet
        , TildeOrdering.rule_ViaMSet
        , TildeOrdering.rule_TildeLeq
        ]
    ,
        [ DontCare.rule_Bool
        , DontCare.rule_Int
        , DontCare.rule_Unnamed
        , DontCare.rule_Tuple
        , DontCare.rule_Record
        , DontCare.rule_Variant
        , DontCare.rule_Permutation
        , DontCare.rule_Matrix
        , DontCare.rule_Abstract
        ]
    ,
        [ rule_TrueIsNoOp
        , rule_FlattenOf1D
        , rule_Decompose_AllDiff
        , rule_Decompose_AllDiff_MapToSingleInt

        , rule_GeneratorsFirst
        ]
    ,
        [ rule_DomainCardinality
        , rule_DomainMinMax

        , rule_ComplexAbsPat

        , rule_AttributeToConstraint

        , rule_QuantifierShift
        , rule_QuantifierShift2
        , rule_QuantifierShift3

        ]

    ,   [ rule_Comprehension_Simplify
        ]

    ,   [ rule_InlineConditions
        , rule_InlineConditions_AllDiff
        , rule_InlineConditions_MaxMin
        ]
    ]

-- | These rules depend on other rules firing first.
delayedRules :: [[Rule]]
delayedRules =
    [
        [ Vertical.Matrix.rule_Comprehension_Singleton
        , Vertical.Matrix.rule_Comprehension_SingletonDomain
        , Vertical.Matrix.rule_Concatenate_Singleton
        , Vertical.Matrix.rule_MatrixIndexing

        ]
    ,   [ rule_ReducerToComprehension
        ]
    ,   [ rule_QuickPermutationOrder
        , rule_DotLtLeq
        , rule_Flatten_Lex
        ]
    ]


rule_ChooseRepr :: (?typeCheckerMode :: TypeCheckerMode) => Config -> Rule
rule_ChooseRepr config = Rule "choose-repr" (const theRule) where

    theRule (Reference nm (Just (DeclNoRepr forg _ inpDom region))) | forg `elem` [Find, Given, CutFind] = do
        let reprsWhichOrder
                | (forg, representationsGivens config) == (Given, Sparse) = reprsSparseOrder
                | (forg, representationsFinds  config) == (Find , Sparse) = reprsSparseOrder
                | representationLevels config == False                    = reprsStandardOrderNoLevels
                | otherwise                                               = reprsStandardOrder
        domOpts <- reprOptions reprsWhichOrder inpDom
        when (null domOpts) $
            bug $ "No representation matches this beast:" <++> pretty inpDom
        let options =
                [ RuleResult { ruleResultDescr = msg
                             , ruleResultType = case forg of
                                    Find    -> ChooseRepr_Find nm
                                    Given   -> ChooseRepr_Given nm
                                    CutFind -> ChooseRepr_Cut nm
                                    _       -> bug "rule_ChooseRepr ruleResultType"
                             , ruleResult = return out
                             , ruleResultHook = Just hook
                             }
                | thisDom <- domOpts
                , let msg = "Choosing representation for" <+> pretty nm <> ":" <++> pretty thisDom
                , let out = Reference nm (Just (DeclHasRepr forg nm thisDom))
                , let hook = mkHook (channelling config) forg nm thisDom region
                ]
        return options
    theRule _ = na "rule_ChooseRepr"

    mkHook
        :: ( MonadLog m
           , MonadFail m
           , MonadFailDoc m
           , NameGen m
           , EnumerateDomain m
           )
        => Bool
        -> FindOrGiven
        -> Name
        -> Domain HasRepresentation Expression
        -> Region
        -> Model
        -> m Model
    mkHook useChannelling   -- whether to use channelling or not
           forg             -- find or given
           name             -- name of the original declaration
           domain           -- domain with representation selected
           region           -- the region of the Reference we are working on
           model = do
        let

            representations     = model |> mInfo |> miRepresentations
            representationsTree = model |> mInfo |> miRepresentationsTree
                                        |> concatMap (\ (n, ds) -> map (n,) ds )

            usedBefore = (name, reprTree domain) `elem` representationsTree

            mkStructurals :: (MonadLog m, MonadFailDoc m, NameGen m, EnumerateDomain m)
                          => m [Expression]
            mkStructurals = do
                let ref = Reference name (Just (DeclHasRepr forg name domain))
                logDebugVerbose $ "Generating structural constraints for:" <+> vcat [pretty ref, pretty domain]
                structurals <- getStructurals downX1 domain >>= \ gen -> gen ref
                logDebugVerbose $ "Before name resolution:" <+> vcat (map pretty structurals)
                resolved    <- mapM resolveNamesX structurals     -- re-resolving names
                logDebugVerbose $ "After  name resolution:" <+> vcat (map pretty resolved)
                return resolved

            addStructurals :: (MonadLog m, MonadFailDoc m, NameGen m, EnumerateDomain m)
                           => Model -> m Model
            addStructurals
                | forg == Given = return
                | usedBefore = return
                | otherwise = \ m -> do
                    structurals <- mkStructurals
                    return $ if null structurals
                        then m
                        else m { mStatements = mStatements m ++ [SuchThat structurals] }

            channels =
                [ make opEq this that
                | (n, d) <- representations
                , n == name
                , let this = Reference name (Just (DeclHasRepr forg name domain))
                , let that = Reference name (Just (DeclHasRepr forg name d))
                ]

            addChannels
                | forg == Given = return
                | usedBefore = return
                | null channels = return
                | otherwise = \ m -> return
                    m { mStatements = mStatements m ++ [SuchThat channels] }

            recordThis
                | usedBefore = return
                | otherwise = \ m ->
                let
                    oldInfo = mInfo m
                    newInfo = oldInfo
                        { miRepresentations     =  representations     ++ [(name, domain)]
                        , miRepresentationsTree = (representationsTree ++ [(name, reprTree domain)])
                                                |> sortBy (comparing fst)
                                                |> groupBy ((==) `on` fst)
                                                |> map (\ grp -> (fst (head grp), map snd grp) )
                        }
                in  return m { mInfo = newInfo }

            fixReprForAllOthers
                | useChannelling = return           -- no-op, if channelling=yes
                | otherwise = \ m ->
                let
                    f (Reference nm _)
                        | nm == name
                        = Reference nm (Just (DeclHasRepr forg name domain))
                    f x = x
                in
                    return m { mStatements = transformBi f (mStatements m) }

            fixReprForSameRegion
                | region == NoRegion = return       -- no-op, if we aren't in a particular region
                | otherwise = \ m ->
                let
                    f (Reference nm (Just (DeclNoRepr _ _ _ region')))
                        | nm == name
                        , region' == region
                        = Reference nm (Just (DeclHasRepr forg name domain))
                    f x = x
                in
                    return m { mStatements = transformBi f (mStatements m) }


        logDebugVerbose $ vcat
            [ "Name        :" <+> pretty name
            , "Previously  :" <+> vcat [ pretty (show d) | (n,d) <- representations, n == name ]
            , "This guy    :" <+> pretty (show domain)
            , "usedBefore? :" <+> pretty usedBefore
            ]

        return model
            >>= addStructurals       -- unless usedBefore: add structurals
            >>= addChannels          -- for each in previously recorded representation
            >>= recordThis           -- unless usedBefore: record (name, domain) as being used in the model
            >>= fixReprForAllOthers  -- fix the representation of this guy in the whole model, if channelling=no
            >>= fixReprForSameRegion -- fix the representation of this guy in the whole model,
                                     -- for those references with the same "region"
            >>= resolveNames         -- we need to re-resolve names to avoid repeatedly selecting representations
                                     -- for abstract stuff inside aliases.


rule_ChooseReprForComprehension :: Config -> Rule
rule_ChooseReprForComprehension config = Rule "choose-repr-for-comprehension" (const theRule) where

    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (nm, domain), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenDomainNoRepr (Single nm) domain) -> return (nm, domain)
            _ -> na "rule_ChooseReprForComprehension"

        let reprsWhichOrder
                | representationsGivens config == Sparse = reprsSparseOrder
                | representationLevels config == False   = reprsStandardOrderNoLevels
                | otherwise                              = reprsStandardOrder
        domOpts <- reprOptions reprsWhichOrder domain
        when (null domOpts) $
            bug $ "No representation matches this beast:" <++> pretty domain

        return
            [ RuleResult
                { ruleResultDescr = "Choosing representation for quantified variable" <+>
                                        pretty nm <> ":" <++> pretty thisDom
                , ruleResultType = ChooseRepr_Quantified
                , ruleResult = bugFailT "rule_ChooseReprForComprehension" $ do
                    outDomains <- downD (nm, thisDom)
                    structurals <- mkStructurals nm thisDom
                    let updateRepr (Reference nm' _)
                            | nm == nm'
                            = Reference nm (Just (DeclHasRepr Quantified nm thisDom))
                        updateRepr p = p
                    let out' = Comprehension (transform updateRepr body)
                                $  gocBefore
                                ++ [ Generator (GenDomainHasRepr name dom)
                                   | (name, dom) <- outDomains ]
                                ++ map Condition structurals
                                ++ transformBi updateRepr gocAfter
                    out <- resolveNamesX out'
                    return out
                , ruleResultHook = Nothing
                }
            | thisDom <- domOpts
            ]
    theRule _ = na "rule_ChooseReprForComprehension"

    mkStructurals name domain = do
        let ref = Reference name (Just (DeclHasRepr Quantified name domain))
        gen  <- getStructurals downX1 domain
        gen ref


rule_ChooseReprForLocals :: Config -> Rule
rule_ChooseReprForLocals config = Rule "choose-repr-for-locals" (const theRule) where

    theRule (WithLocals body (AuxiliaryVars locals)) = do
        (stmtBefore, (nm, domain), stmtAfter) <- matchFirst locals $ \ local -> case local of
            Declaration (FindOrGiven LocalFind nm domain) -> return (nm, domain)
            _ -> na "rule_ChooseReprForLocals"

        let
            isReferencedWithoutRepr (Reference nm' (Just DeclNoRepr{})) | nm == nm' = True
            isReferencedWithoutRepr _ = False

        unless (any isReferencedWithoutRepr (universeBi (body, stmtBefore, stmtAfter))) $
            na $ "This local variable seems to be handled before:" <+> pretty nm

        let reprsWhichOrder
                | representationsAuxiliaries config == Sparse   = reprsSparseOrder
                | representationLevels config == False          = reprsStandardOrderNoLevels
                | otherwise                                     = reprsStandardOrder
        domOpts <- reprOptions reprsWhichOrder domain
        when (null domOpts) $
            bug $ "No representation matches this beast:" <++> pretty domain

        return
            [ RuleResult
                { ruleResultDescr = "Choosing representation for auxiliary variable" <+>
                                        pretty nm <> ":" <++> pretty thisDom
                , ruleResultType = ChooseRepr_Auxiliary
                , ruleResult = bugFailT "rule_ChooseReprForLocals" $ do
                    outDomains <- downD (nm, thisDom)
                    structurals <- mkStructurals nm thisDom
                    let updateRepr (Reference nm' _)
                            | nm == nm'
                            = Reference nm (Just (DeclHasRepr LocalFind nm thisDom))
                        updateRepr p = p
                    let out' = WithLocals (transform updateRepr body) $ AuxiliaryVars
                                (  stmtBefore
                                ++ [ Declaration (FindOrGiven
                                                    LocalFind
                                                    name
                                                    (forgetRepr dom))
                                   | (name, dom) <- outDomains ]
                                ++ [ SuchThat structurals | not (null structurals) ]
                                ++ transformBi updateRepr stmtAfter
                                )
                    out <- resolveNamesX out'
                    return out
                , ruleResultHook = Nothing
                }
            | thisDom <- domOpts
            ]
    theRule _ = na "rule_ChooseReprForLocals"

    mkStructurals name domain = do
        let ref = Reference name (Just (DeclHasRepr LocalFind name domain))
        gen  <- getStructurals downX1 domain
        gen ref


rule_GeneratorsFirst :: Rule
rule_GeneratorsFirst = "generators-first" `namedRule` theRule where
    theRule (Comprehension body [])
        = return
            ( "Empty generators."
            , return $ AbstractLiteral $ AbsLitMatrix (mkDomainIntB 1 1) [body]
            )
    theRule (Comprehension body gensOrConds)
        | let (gens, rest) = mconcat
                [ case x of
                    Generator{} -> ([x],[])
                    _           -> ([],[x])
                | x <- gensOrConds
                ]
        , let gensOrConds' = gens ++ rest
        , gensOrConds /= gensOrConds'
        = return
            ( "Generators come first."
            , return $ Comprehension body gensOrConds'
            )
    theRule (Comprehension body gensOrConds)
        | let (lettings :: [Name], rest :: [GeneratorOrCondition]) = mconcat
                [ case x of
                    ComprehensionLetting pat _ -> (universeBi pat,[] )
                    _                          -> ([]  ,[x])
                | x <- gensOrConds
                ]
        , let f (Reference nm (Just (Alias x))) | nm `elem` lettings = f x
              f x = x
        , not (null lettings)
        = return
            ( "Inlining comprehension lettings."
            , return $ transformBi f $ Comprehension body rest
            )
    theRule _ = na "rule_GeneratorsFirst"


rule_Eq :: Rule
rule_Eq = "identical-domain-eq" `namedRule` theRule where
    theRule p = do
        (x,y) <- match opEq p
        domX  <- domainOf x
        domY  <- domainOf y
        unless (domX == domY) $ na "rule_Eq domains not identical"
        sameRepresentationTree x y
        xs <- downX x
        ys <- downX y
        unless (length xs == length ys) $ na "rule_Eq"
        when (xs == [x]) $ na "rule_Eq"
        when (ys == [y]) $ na "rule_Eq"
        return
            ( "Generic vertical rule for identical-domain equality"
            , return $ make opAnd $ fromList $ zipWith (\ i j -> [essence| &i = &j |] ) xs ys
            )


rule_Neq :: Rule
rule_Neq = "identical-domain-neq" `namedRule` theRule where
    theRule p = do
        (x,y) <- match opNeq p
        domX  <- domainOf x
        domY  <- domainOf y
        unless (domX == domY) $ na "rule_Neq domains not identical"
        sameRepresentationTree x y
        xs <- downX x
        ys <- downX y
        unless (length xs == length ys) $ na "rule_Neq"
        when (xs == [x]) $ na "rule_Neq"
        when (ys == [y]) $ na "rule_Neq"
        return
            ( "Generic vertical rule for identical-domain equality"
            , return $ make opOr $ fromList $ zipWith (\ i j -> [essence| &i != &j |] ) xs ys
            )


rule_QuickPermutationOrder :: Rule
rule_QuickPermutationOrder = "generic-QuickPermutationOrder" `namedRule` theRule where
    -- theRule p@[essence| quickPermutationOrder(&x, &ps) |] = do
    --     -- case listOut ps of
    --     --     Just [perm] ->
    --     --         return
    --     --             ( "Generic vertical rule for quickPermutationOrder:" <+> pretty perm
    --     --             , return [essence| &x .<= transform(&perm, &x) |]
    --     --             )
    --     --     _ -> na "rule_QuickPermutationOrder - not implemented for multiple permutations yet"
    --     traceM $ show $ "HERE x " <+> pretty x
    --     traceM $ show $ "HERE ps" <+> pretty ps
    --     x_ord <- symmetryOrdering x
    --     -- x_perm <- symmetryOrdering [essence| transform(&ps, &x) |]
    --     traceM $ show $ "HERE x_ord" <+> pretty x_ord
    --     return
    --         ( "Generic vertical rule for quickPermutationOrder:" <+> pretty p
    --         , return [essence| &x_ord .<= transform(&ps, &x_ord) |]
    --         )
    theRule _ = na "rule_QuickPermutationOrder"


rule_DotLtLeq :: Rule
rule_DotLtLeq = "generic-DotLtLeq" `namedRule` theRule where
    theRule p = do
        (a,b,mk) <- case p of
                    [essence| &a .<  &b |] -> return ( a, b, \ i j -> [essence| &i <lex  &j |] )
                    [essence| &a .<= &b |] -> return ( a, b, \ i j -> [essence| &i <=lex &j |] )
                    _ -> na "rule_DotLtLeq"
        ma <- symmetryOrdering a
        mb <- symmetryOrdering b
        return
            ( "Generic vertical rule for dotLt and dotLeq:" <+> pretty p
            , return $ mk ma mb
            )


rule_Flatten_Lex :: Rule
rule_Flatten_Lex = "flatten-lex" `namedRule` theRule where
    theRule [essence| &a <lex &b |] = do
      reject_flat a b
      fa <- flatten a
      fb <- flatten b
      tfa <- typeOf fa
      tfb <- typeOf fb
      case (tfa, tfb) of
        (TypeList TypeInt{}, TypeList TypeInt{}) -> return ()
        (TypeMatrix TypeInt{} TypeInt{}, TypeMatrix TypeInt{} TypeInt{}) -> return ()
        _ -> bug $ "flattener: " <+> vcat [stringToDoc $ show tfa, stringToDoc $ show tfb]
      return ( "Flatten Lex less"
             , return [essence| &fa <lex &fb |]
             )
    theRule [essence| &a <=lex &b |] = do
      reject_flat a b
      fa <- flatten a
      fb <- flatten b
      tfa <- typeOf fa
      tfb <- typeOf fb
      case (tfa, tfb) of
        (TypeList TypeInt{}, TypeList TypeInt{}) -> return ()
        (TypeMatrix TypeInt{} TypeInt{}, TypeMatrix TypeInt{} TypeInt{}) -> return ()
        _ -> bug $ "flattener: " <+> vcat [stringToDoc $ show tfa, stringToDoc $ show tfb]
      return ( "Flatten Lex Lt"
             , return [essence| &fa <=lex &fb |]
             )
    theRule _ = na "rule_Flatten_Lex"
    reject_flat a b = do
      ta <- typeOf a
      tb <- typeOf b
      case (ta, tb) of
        (TypeMatrix TypeBool TypeInt{}, _) ->
          na "rule_Flatten_Lex"
        (TypeMatrix TypeBool TypeBool, _) ->
          na "rule_Flatten_Lex"
        (TypeList TypeInt{}, _) ->
          na "rule_Flatten_Lex"
        (TypeMatrix TypeInt{} TypeInt{}, _) ->
          na "rule_Flatten_Lex"
        (TypeList TypeBool, _) ->
          na "rule_Flatten_Lex"
        (TypeMatrix TypeInt{} TypeBool, _) ->
          na "rule_Flatten_Lex"
        _ -> return ()

    flatten a = do
      ta <- typeOf a
      case ta of
        TypeBool -> return [essence| [-toInt(&a)] |]
        TypeInt{} -> return [essence| [&a] |]
        TypeList TypeInt{} -> return a
        TypeMatrix TypeInt{} TypeInt{} -> return a
        TypeTuple ts -> do
          case a of
            AbstractLiteral x -> do
              case x of
                AbsLitTuple xs -> do
                  fxs <- mapM flatten xs
                  let flatxs = fromList fxs
                  return [essence| flatten(&flatxs) |]
                _ -> bug $ "rule_FlattenLex: flatten isn't defined for this abslit fellow..."
                    <+> vcat [pretty a, pretty ta, stringToDoc $ show a]
            Constant c ->
              case c of
                ConstantAbstract ca ->
                  case ca of
                    AbsLitTuple xs -> do
                      fxs <- mapM flatten (Constant <$> xs)
                      let flatxs = fromList fxs
                      return [essence| flatten(&flatxs) |]
                    _ -> bug $ "rule_FlattenLex: flatten isn't defined for this constant fellow..."
                        <+> vcat [pretty a, pretty ta, stringToDoc $ show a]
                _ -> bug $ "rule_FlattenLex: flatten isn't defined for this constant fellow..."
                    <+> vcat [pretty a, pretty ta, stringToDoc $ show a]
            Op _ -> do
              (oName, o) <- quantifiedVar
              flatten $ Comprehension o [ComprehensionLetting oName a]
            _ -> do
              ps <- mapM (\(i,_) -> do
                                  (Single nm, tm) <- quantifiedVar
                                  return (i,nm,tm)) (zip [1..] ts)
              let lts = (\(i,nm,_tm) -> ComprehensionLetting (Single nm) [essence| &a[&i] |]) <$> ps
                  tup = AbstractLiteral $ AbsLitTuple $ (\(_,_,tm) -> tm) <$> ps
              flatten $ Comprehension tup lts
        _ ->
          case a of
            AbstractLiteral x -> do
              case x of
                AbsLitMatrix _ xs -> do
                  fxs <- mapM flatten xs
                  let flatxs = fromList fxs
                  return [essence| flatten(&flatxs) |]
                _ -> bug $ "rule_FlattenLex: flatten isn't defined for this abslit fellow..."
                    <+> vcat [pretty a, pretty ta, stringToDoc $ show a]
            Constant c ->
              case c of
                ConstantAbstract ca ->
                  case ca of
                    AbsLitMatrix _ [] ->
                      return [essence| ([] : `matrix indexed by [int()] of int`) |]
                    AbsLitMatrix _ xs -> do
                      fxs <- mapM flatten (Constant <$> xs)
                      let flatxs = fromList fxs
                      return [essence| flatten(&flatxs) |]
                    _ -> bug $ "rule_FlattenLex: flatten isn't defined for this constant fellow..."
                        <+> vcat [pretty a, pretty ta, stringToDoc $ show a]
                TypedConstant tc _ -> flatten (Constant tc)
                _ -> bug $ "rule_FlattenLex: flatten isn't defined for this constant fellow..."
                    <+> vcat [pretty a, pretty ta, stringToDoc $ show a]
            Op _ -> do
              (oName, o) <- quantifiedVar
              flatten $ Comprehension o [ComprehensionLetting oName a]
            Reference nm ex ->
                  bug $ "rule_FlattenLex: flatten isn't defined for this reference fellow..."
                     <+> vcat [stringToDoc (show a)
                              ,"reference:" <+> stringToDoc (show nm)
                              ,"fellow:" <+> stringToDoc (show ex)]
            Comprehension body gocs -> do
              fbody <- flatten body
              let comp = Comprehension fbody gocs
              return [essence| flatten(&comp) |]
            _ -> bug $ "rule_FlattenLex: flatten isn't defined for this expression fellow..."

                    <+> vcat [pretty a, pretty ta, stringToDoc $ show a]


rule_ReducerToComprehension :: Rule
rule_ReducerToComprehension = "reducer-to-comprehension" `namedRule` theRule where
    theRule p = do
        (_, _, mk, coll) <- match opReducer p
        -- leave comprehensions alone
        let
            isComprehension Comprehension{} = True
            isComprehension _ = False
        case followAliases isComprehension coll of
            True  -> na "rule_ReducerToComprehension"
            False -> return ()
        -- leave matrix literals alone
        case tryMatch matrixLiteral coll of
            Nothing -> return ()
            Just {} -> na "rule_ReducerToComprehension"
        tyColl <- typeOf coll
        howToIndex <- case tyColl of
            TypeSequence{} -> return $ Left ()
            TypeMatrix{}   -> return $ Right ()
            TypeList{}     -> return $ Right ()
            TypeSet{}      -> return $ Right ()
            TypeMSet{}     -> return $ Right ()
            _ -> na "rule_ReducerToComprehension"
        return
            ( "Creating a comprehension for the collection inside the reducer operator."
            , do
                (iPat, i) <- quantifiedVar
                case howToIndex of
                    Left{}  -> return $ mk [essence| [ &i[2] | &iPat <- &coll ] |]
                    Right{} -> return $ mk [essence| [ &i    | &iPat <- &coll ] |]
            )


rule_TrueIsNoOp :: Rule
rule_TrueIsNoOp = "true-is-noop" `namedRule` theRule where
    theRule (Op (MkOpTrue (OpTrue ref))) =
        case ref of
            Reference _ (Just DeclHasRepr{}) ->
                return ( "Remove the argument from true."
                       , return $ Constant $ ConstantBool True
                       )
            _ -> na "The argument of true doesn't have a representation."
    theRule _ = na "rule_TrueIsNoOp"


rule_FlattenOf1D :: Rule
rule_FlattenOf1D = "flatten-of-1D" `namedRule` theRule where
    theRule p = do
        x   <- match opFlatten p
        tyx <- typeOf x
        out <- case tyx of
            TypeList     TypeBool{} -> return x
            TypeList     TypeInt{}  -> return x
            TypeMatrix _ TypeBool{} -> return x
            TypeMatrix _ TypeInt{}  -> return x
            TypeMatrix{}            -> -- more than 1D
                case listOut x of
                    Just [y] -> return (make opFlatten y)
                    _        -> na "rule_FlattenOf1D"
            _ -> na "rule_FlattenOf1D"
        return ( "1D matrices do not need a flatten."
               , return out
               )


rule_Decompose_AllDiff :: Rule
rule_Decompose_AllDiff = "decompose-allDiff" `namedRule` theRule where
    theRule [essence| allDiff(&m) |] = do
        ty <- typeOf m
        case ty of
            TypeMatrix _ TypeBool -> na "allDiff can stay"
            TypeMatrix _ (TypeInt _) -> na "allDiff can stay"
            TypeMatrix _ _        -> return ()
            _                     -> na "allDiff on something other than a matrix."
        index:_ <- indexDomainsOf m
        return
            ( "Decomposing allDiff. Type:" <+> pretty ty
            , do
                (iPat, i) <- quantifiedVar
                (jPat, j) <- quantifiedVar
                return
                    [essence|
                        and([ &m[&i] != &m[&j]
                            | &iPat : &index
                            , &jPat : &index
                            , &i < &j
                            ])
                    |]
            )
    theRule _ = na "rule_Decompose_AllDiff"


rule_Decompose_AllDiff_MapToSingleInt :: Rule
rule_Decompose_AllDiff_MapToSingleInt = "decompose-allDiff-mapToSingleInt" `namedRule` theRule where
    theRule [essence| allDiff(&m) |] = do
        case m of
            Comprehension body gensOrConds -> do
                tyBody <- typeOf body
                case tyBody of
                    TypeBool -> na "rule_Decompose_AllDiff_MapToSingleInt"
                    TypeInt _ -> na "rule_Decompose_AllDiff_MapToSingleInt"
                    TypeTuple{} -> do
                        bodyBits <- downX1 body
                        bodyBitSizes <- forM bodyBits $ \ b -> do
                            bDomain <- domainOf b
                            domainSizeOf bDomain
                        case (bodyBits, bodyBitSizes) of
                            ([a,b], [_a',b']) -> do
                                let body'=  [essence| &a * &b' + &b |]
                                let m' = Comprehension body' gensOrConds
                                return
                                    ( "Decomposing allDiff"
                                    , return [essence| allDiff(&m') |]
                                    )
                            _ -> na "rule_Decompose_AllDiff_MapToSingleInt"
                    _ -> na "allDiff on something other than a comprehension."
            _ -> na "allDiff on something other than a comprehension."
    theRule _ = na "rule_Decompose_AllDiff_MapToSingleInt"


rule_DomainCardinality :: Rule
rule_DomainCardinality = "domain-cardinality" `namedRule` theRule where
    theRule p = do
        maybeDomain <- match opTwoBars p
        d <- expandDomainReference <$> case maybeDomain of
            Domain d -> return d
            Reference _ (Just (Alias (Domain d))) -> return d
            _ -> na "rule_DomainCardinality"
        return
            ( "Cardinality of a domain"
            , case d of
                DomainInt _ [RangeBounded 1 u] -> return u
                _ -> do
                    (iPat, _) <- quantifiedVar
                    return [essence| sum([ 1 | &iPat : &d ]) |]
            )


rule_DomainMinMax :: Rule
rule_DomainMinMax = "domain-MinMax" `namedRule` theRule where
    theRule [essence| max(&maybeDomain) |] = do
        d <- getDomain maybeDomain
        return
            ( "max of a domain"
            , maxOfDomain d
            )
    theRule [essence| min(&maybeDomain) |] = do
        d <- getDomain maybeDomain
        return
            ( "min of a domain"
            , minOfDomain d
            )
    theRule _ = na "rule_DomainMinMax"

    getDomain :: MonadFailDoc m => Expression -> m (Domain () Expression)
    getDomain (Domain d) = return d
    getDomain (Reference _ (Just (Alias (Domain d)))) = getDomain (Domain d)
    getDomain _ = na "rule_DomainMinMax.getDomain"


rule_ComplexAbsPat :: Rule
rule_ComplexAbsPat = "complex-pattern" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, domainOrExpr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenDomainNoRepr pat@AbsPatTuple{} domain) -> return (pat, Left domain)
            Generator (GenInExpr       pat@AbsPatTuple{} expr)   -> return (pat, Right expr)
            _ -> na "rule_ComplexAbsPat"
        return
            ( "complex pattern on tuple patterns"
            , do
                (iPat, i) <- quantifiedVar
                let replacements = [ (p, make opMatrixIndexing i (map (fromInt . fromIntegral) is))
                                   | (p, is) <- genMappings pat
                                   ]
                let f x@(Reference nm _) = fromMaybe x (lookup nm replacements)
                    f x = x
                return $ Comprehension (transform f body)
                            $  gocBefore
                            ++ [ either (Generator . GenDomainNoRepr iPat)
                                        (Generator . GenInExpr       iPat)
                                        domainOrExpr ]
                            ++ transformBi f gocAfter
            )
    theRule _ = na "rule_ComplexAbsPat"

    -- i         --> i -> []
    -- (i,j)     --> i -> [1]
    --               j -> [2]
    -- (i,(j,k)) --> i -> [1]
    --               j -> [2,1]
    --               k -> [2,2]
    genMappings :: AbstractPattern -> [(Name, [Int])]
    genMappings (Single nm) = [(nm, [])]
    genMappings (AbsPatTuple pats)
        = concat
            [ [ (patCore, i:is) | (patCore, is) <- genMappings pat ]
            | (i, pat) <- zip [1..] pats
            ]
    genMappings (AbsPatMatrix pats)
        = concat
            [ [ (patCore, i:is) | (patCore, is) <- genMappings pat ]
            | (i, pat) <- zip [1..] pats
            ]
    genMappings pat = bug ("rule_ComplexLambda.genMappings:" <+> pretty (show pat))


-- this rule doesn't use `namedRule` because it need access to ascendants through the zipper
rule_InlineConditions :: Rule
rule_InlineConditions = Rule "inline-conditions" theRule where
    theRule z (Comprehension body gensOrConds) = do
        let (toInline, toKeep) = mconcat
                [ case goc of
                    Condition x | categoryOf x == CatDecision -> ([x],[])
                    _ -> ([],[goc])
                | goc <- gensOrConds
                ]
        theGuard <- case toInline of
            []  -> na "No condition to inline."
            [x] -> return x
            xs  -> return $ make opAnd $ fromList xs
        (nameQ, opSkip) <- queryQ z
        let bodySkipped = opSkip theGuard body
        return
            [ RuleResult
                { ruleResultDescr = "Inlining conditions, inside" <+> nameQ
                , ruleResultType  = ExpressionRefinement
                , ruleResult      = return $ Comprehension bodySkipped toKeep
                , ruleResultHook  = Nothing
                } ]
    theRule _ _ = na "rule_InlineConditions"

    -- keep going up, until finding a quantifier
    -- when found, return the skipping operator for the quantifier
    -- if none exists, do not apply the rule.
    -- (or maybe we should call bug right ahead, it can't be anything else.)
    queryQ z0 =
        case Zipper.up z0 of
            Nothing -> na "rule_InlineConditions (meh-1)"
            Just z -> do
                let h = hole z
                case ( match opAnd h, match opOr h, match opSum h, match opProduct h
                     , match opMin h, match opMax h, match opOrdering h ) of
                    (Just{}, _, _, _, _, _, _) -> return ("and", opAndSkip)
                    (_, Just{}, _, _, _, _, _) -> return ("or" , opOrSkip )
                    (_, _, Just{}, _, _, _, _) -> return ("sum", opSumSkip)
                    (_, _, _, Just{}, _, _, _) -> return ("product", opProductSkip)
                    (_, _, _, _, Just{}, _, _) -> na "rule_InlineConditions (min)"
                    (_, _, _, _, _, Just{}, _) -> na "rule_InlineConditions (max)"
                    (_, _, _, _, _, _, Just{}) -> return ("ordering", opSumSkip)
                    _                          -> na "rule_InlineConditions (meh-2)"
                                            -- case Zipper.up z of
                                            --     Nothing -> na "queryQ"
                                            --     Just u  -> queryQ u

    opAndSkip     b x = [essence| &b -> &x |]
    opOrSkip      b x = [essence| &b /\ &x |]
    opSumSkip     b x = [essence| toInt(&b) * catchUndef(&x, 0) |]
    opProductSkip b x = [essence| [ 1
                                  , catchUndef(&x,1)
                                  ; int(0..1)
                                  ] [toInt(&b)] |]


rule_InlineConditions_AllDiff :: Rule
rule_InlineConditions_AllDiff = "inline-conditions-allDiff" `namedRule` theRule where
    theRule (Op (MkOpAllDiff (OpAllDiff (Comprehension body gensOrConds)))) = do
        let (toInline, toKeep) = mconcat
                [ case goc of
                    Condition x | categoryOf x == CatDecision -> ([x],[])
                    _ -> ([],[goc])
                | goc <- gensOrConds
                ]
        theGuard <- case toInline of
            []  -> na "No condition to inline."
            [x] -> return x
            xs  -> return $ make opAnd $ fromList xs

        tyBody <- typeOf body
        case tyBody of
            TypeInt{} -> return ()
            _ -> na "rule_InlineConditions_AllDiff, not an int"
        domBody <- domainOf body
        let
            collectLowerBounds (RangeSingle x) = return x
            collectLowerBounds (RangeBounded x _) = return x
            collectLowerBounds _ = userErr1 ("Unexpected infinite domain:" <+> pretty domBody)

            collectLowerBoundsD (DomainInt _ rs) = mapM collectLowerBounds rs
            collectLowerBoundsD _  = userErr1 ("Expected an integer domain, but got:" <+> pretty domBody)

        bounds <- collectLowerBoundsD domBody
        let lowerBound = make opMin (fromList bounds)

        -- for each element, we do element-lowerBound+1
        -- this makes sure the smallest element is 1
        -- hence we can use 0 as the except value!
        let bodySkipped = [essence| toInt(&theGuard) * catchUndef(&body + (1 - &lowerBound), 0) |]

        return
            ( "Inlining conditions, inside allDiff"
            , return $ make opAllDiffExcept (Comprehension bodySkipped toKeep) 0
            )
    theRule _ = na "rule_InlineConditions_AllDiff"


rule_InlineConditions_MaxMin :: Rule
rule_InlineConditions_MaxMin = "aux-for-MaxMin" `namedRule` theRule where
    theRule p = do
        when (categoryOf p < CatDecision) $ na "rule_InlineConditions_MaxMin"
        (nameQ, binOp, Comprehension body gensOrConds) <-
            case (match opMax p, match opMin p) of
                (Just res, _) -> return ("max", \ a b -> [essence| &a <= &b |], res )
                (_, Just res) -> return ("min", \ a b -> [essence| &a >= &b |], res )
                _ -> na "rule_InlineConditions_MaxMin"
        let
            (toInline, gocInExpr, _toKeep) = mconcat
                [ case goc of
                    Condition x | categoryOf x == CatDecision -> ([x],[],[])
                    Generator (GenInExpr {}) -> ([],[goc],[])
                    _ -> ([],[],[goc])
                | goc <- gensOrConds
                ]
        when (null toInline && null gocInExpr) $ na "rule_InlineConditions_MaxMin"
        auxDomain <- domainOf body
        return
            ( "Creating auxiliary variable for a" <+> nameQ
            , do
                (auxName, aux) <- auxiliaryVar
                let auxDefinedLHS = make opSum (Comprehension 1 gensOrConds)
                let auxDefined = [essence| &auxDefinedLHS > 0 |]
                let auxUndefined = [essence| &auxDefinedLHS = 0 |]
                let aux' = WithLocals aux (DefinednessConstraints [auxDefined])
                return $ WithLocals aux'
                    (AuxiliaryVars
                        [ Declaration (FindOrGiven LocalFind auxName auxDomain)
                        , SuchThat
                            [ make opAnd $ Comprehension
                                (binOp body aux)
                                gensOrConds

                        -- either one of the members of this comprehension, or dontCare
                        -- if it is indeed dontCare, care should be taken to make sure it isn't used as a normal value
                            , make opAnd $ fromList
                                [ make opImply auxDefined
                                    (make opOr  $ Comprehension
                                        [essence| &body = &aux |]
                                        gensOrConds)
                                , make opImply auxUndefined (make opDontCare aux)
                                ]
                            ]
                        ])
            )


rule_AttributeToConstraint :: Rule
rule_AttributeToConstraint = "attribute-to-constraint" `namedRule` theRule where
    theRule (Op (MkOpAttributeAsConstraint (OpAttributeAsConstraint thing attr mval))) = do
        dom <- domainOf thing
        let conv = mkAttributeToConstraint dom attr mval thing
        return
            ( "Converting an attribute to a constraint"
            , bugFailT "rule_AttributeToConstraint" conv
            )
    theRule _ = na "rule_AttributeToConstraint"


timedF :: MonadIO m => String -> (a -> m b) -> a -> m b
timedF name comp a = timeItNamed name (comp a)


evaluateModel ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m Model
evaluateModel m = do
    let
        full (Reference _ (Just (DeclHasRepr _ _ (singletonDomainInt -> Just val)))) =
            return val
        full p@Constant{} = return p
        full p@Domain{} = return p
        full p = do
            mconstant <- runExceptT (instantiateExpression [] p)
            case mconstant of
                Left{} -> return p
                Right constant ->
                    if null [() | ConstantUndefined{} <- universe constant] -- if there are no undefined values in it
                        then return (Constant constant)
                        else return p
    let
        partial (Op op)
            | Just (x, y) <- case op of
                                MkOpLeq (OpLeq x y) -> Just (x,y)
                                MkOpGeq (OpGeq x y) -> Just (x,y)
                                MkOpEq  (OpEq  x y) -> Just (x,y)
                                _                   -> Nothing
            , Reference nmX _ <- x
            , Reference nmY _ <- y
            , nmX == nmY
            , categoryOf x <= CatQuantified
            , categoryOf y <= CatQuantified
            = return (fromBool True)
        partial p@(Op x) = do
            mx' <- runExceptT (simplifyOp x)
            case mx' of
                Left{} -> return p
                Right x' -> do
                    when (Op x == x') $ bug $ vcat
                        [ "rule_PartialEvaluate, simplifier returns the input unchanged."
                        , "input:" <+> vcat [ pretty (Op x)
                                            , pretty (show (Op x))
                                            ]
                        ]
                    return x'
        partial p = return p

    (descendBiM full >=> transformBiM partial) m


rule_FullEvaluate :: Rule
rule_FullEvaluate = "full-evaluate" `namedRule` theRule where
    theRule Constant{} = na "rule_FullEvaluate"
    theRule Domain{} = na "rule_FullEvaluate"
    theRule (Reference _ (Just (Alias x)))                 -- selectively inline, unless x is huge
        | Just Comprehension{} <- match opToSet x
        = return ("Inline alias", return x)
    theRule p = do
        constant <- instantiateExpression [] p
        unless (null [() | ConstantUndefined{} <- universe constant]) $
            na "rule_PartialEvaluate, undefined"
        return ("Full evaluator", return $ Constant constant)


rule_PartialEvaluate :: Rule
rule_PartialEvaluate = "partial-evaluate" `namedRuleZ` theRule where
    -- if a variable only has a single value in its domain, replace it with the value
    theRule z (Reference _ (Just (DeclHasRepr _ _ (singletonDomainInt -> Just val)))) =
        case hole <$> Zipper.up z of
            Just (Op (MkOpTrue _)) -> na "rule_PartialEvaluate, inside a true(ref)"
            _                      -> return ( "Partial evaluator"
                                             , return val
                                             )
    theRule _ (Op op)
        | Just (x, y) <- case op of
                            MkOpLeq (OpLeq x y) -> Just (x,y)
                            MkOpGeq (OpGeq x y) -> Just (x,y)
                            MkOpEq  (OpEq  x y) -> Just (x,y)
                            _                   -> Nothing
        , Reference nmX _ <- x
        , Reference nmY _ <- y
        , nmX == nmY
        , categoryOf x <= CatQuantified
        , categoryOf y <= CatQuantified
        = return
            ( "Parameter = parameter (or quantified)"
            , return (fromBool True)
            )
    theRule _ (Op x) = do
        x' <- simplifyOp x
        when (Op x == x') $ bug $ vcat
            [ "rule_PartialEvaluate, simplifier returns the input unchanged."
            , "input:" <+> vcat [ pretty (Op x)
                                , pretty (show (Op x))
                                ]
            ]
        return
            ( "Partial evaluator"
            , return x'
            )
    theRule _ _ = na "rule_PartialEvaluate"


-- | shifting quantifiers inwards, if they operate on a row of a 2d matrix,
--   make them operate on the rows directly then index
rule_QuantifierShift :: Rule
rule_QuantifierShift = "quantifier-shift" `namedRule` theRule where
    theRule p = do
        (_, _, mkQuan, inner)           <- match opReducer p
        (matrix, indexer)               <- match opIndexing inner
        (TypeMatrix _ ty, index, elems) <- match matrixLiteral matrix
        case ty of
            TypeMatrix{} -> return ()
            TypeList{} -> return ()
            _ -> na "rule_QuantifierShift"
        return
            ( "Shifting quantifier inwards"
            , return $ make opIndexing
                        (make matrixLiteral
                            ty
                            index
                            (map mkQuan elems))
                        indexer
            )


-- | shifting quantifiers inwards, if they operate on a flattened multi-dim matrix.
rule_QuantifierShift2 :: Rule
rule_QuantifierShift2 = "quantifier-shift2" `namedRule` theRule where
    theRule p = do
        (_, _, mkQuan, inner)           <- match opReducer p
        matrix                          <- match opFlatten inner
        (TypeMatrix _ ty, index, elems) <- match matrixLiteral matrix
        case ty of
            TypeMatrix{} -> return ()           -- the matrix literal should contain further matrix/list stuff.
            TypeList{}   -> return ()
            _            -> na "rule_QuantifierShift2"
        return
            ( "Shifting quantifier inwards"
            , return $ mkQuan
                        (make matrixLiteral
                            ty
                            index
                            (map (mkQuan . flattenIfNeeded (matrixNumDims ty)) elems))
            )


-- | shifting quantifiers inwards, if they operate on a concatenated multi-dim matrix.
rule_QuantifierShift3 :: Rule
rule_QuantifierShift3 = "quantifier-shift3" `namedRule` theRule where
    theRule p = do
        (_, True, mkQuan, inner)        <- match opReducer p
        matrix                          <- match opConcatenate inner
        (TypeMatrix _ ty, index, elems) <- match matrixLiteral matrix
        return
            ( "Shifting quantifier inwards"
            , return $ mkQuan $ make matrixLiteral
                                        ty
                                        index
                                        (map mkQuan elems)
            )


rule_Comprehension_Simplify :: Rule
rule_Comprehension_Simplify = "comprehension-simplify" `namedRule` theRule where
    theRule (Comprehension x gocs)
        | let isTrueCondition (Condition (Constant (ConstantBool True))) = True
              isTrueCondition _ = False
        , let gocs' = filter (not . isTrueCondition) gocs
        , length gocs' < length gocs
        = return
            ( "Removing true conditions"
            , return $ Comprehension x gocs'
            )
    theRule _ = na "rule_Comprehension_Simplify"


rule_Xor_To_Sum :: Rule
rule_Xor_To_Sum = "xor-to-sum" `namedRule` theRule where
    theRule [essence| xor(&arg) |] =
        case arg of
            Comprehension body goc -> do
                let argOut = Comprehension [essence| toInt(&body) |] goc
                return
                    ( "xor to sum"
                    , return [essence| 1 = sum(&argOut) % 2 |]
                    )
            AbstractLiteral (AbsLitMatrix dom elems) -> do
                let argOut = AbstractLiteral $ AbsLitMatrix dom
                                [ [essence| toInt(&el) |] | el <- elems ]
                return
                    ( "xor to sum"
                    , return [essence| 1 = sum(&argOut) % 2 |]
                    )
            _ -> do
                (iPat, i) <- quantifiedVar
                return
                    ( "xor to sum"
                    , return [essence| 1 = sum([ toInt(&i) | &iPat <- &arg ]) % 2 |]
                    )
    theRule _ = na "rule_Xor_To_Sum"


enforceTagConsistency :: MonadFail m => Model -> m Model
enforceTagConsistency model = do
  let statements' = transformBi reDomExp $ transformBi reDomConst (mStatements model)
  return model { mStatements = statements' }


addUnnamedSymmetryBreaking ::
    NameGen m =>
    Maybe UnnamedSymmetryBreaking ->
    Model ->
    m Model
addUnnamedSymmetryBreaking mode model = do

    let
        allUnnamedTypes :: [(Domain () Expression, Expression)]
        allUnnamedTypes =
            [ reTag (TagUnnamed nm') (DomainReference nm Nothing,  x) --x is a TagInt at this point so we must reTag it
            | Declaration (LettingDomainDefnUnnamed nm@(Name nm') x) <- mStatements model
            ]

        allDecVars =
            [ (Reference nm Nothing, domain)
            | Declaration (FindOrGiven Find nm domain) <- mStatements model
            ]

        -- allDecVarsAux auxSuffix =
        --     [ (Reference (mconcat [nm, "_auxFor_", auxSuffix]) Nothing, domain)
        --     | Declaration (FindOrGiven Find nm domain) <- mStatements model
        --     ]

        varsTuple = case allDecVars of
                        [v] -> fst v
                        _ -> AbstractLiteral $ AbsLitTuple $ map fst allDecVars
        -- mkAuxTuple auxSuffix = AbstractLiteral $ AbsLitTuple $ map fst (allDecVarsAux auxSuffix)

--    traceM $ show $ "Unnamed types in this model:" <++> prettyList id "," allUnnamedTypes
--    traceM $ show $ "Unnamed decision variables in this model:" <++> prettyList id "," allDecVars

    -- 3 axis of doom
    -- 1. Quick/Complete. Quick is quickPermutationOrder(x, p)   -- this is an efficient subset of x .<= p(x)
    --                    Complete is x .<= p(x)
    -- 2. Scope.          Consecutive
    --                    AllPairs
    --                    AllPermutations
    -- 3. Independently/Altogether

    case mode of
        Nothing -> return model
        Just (UnnamedSymmetryBreaking quickOrComplete usbScope independentlyOrAltogether) -> do
            -- let newDecls =
            --         case quickOrComplete of
            --             USBQuick -> []
            --             USBComplete ->
            --                 case independentlyOrAltogether of
            --                     USBIndependently ->
            --                         [ Declaration (FindOrGiven LocalFind nm' domain)
            --                         | Declaration (FindOrGiven Find nm  domain) <- mStatements model
            --                         , (DomainReference uName _, _) <- allUnnamedTypes
            --                         , let nm' = mconcat [nm, "_auxFor_", uName]
            --                         ]
            --                     USBAltogether ->
            --                         [ Declaration (FindOrGiven LocalFind nm' domain)
            --                         | Declaration (FindOrGiven Find nm  domain) <- mStatements model
            --                         , let nm' = mconcat [nm, "_auxFor_all"]
            --                         ]

            let

                buildPermutationChain = make opTransform

                combinedPermApply perms =
                    case quickOrComplete of
                        USBQuick -> make opQuickPermutationOrder perms varsTuple
                        USBComplete ->
                            let applied = buildPermutationChain perms varsTuple
                            in [essence| &varsTuple .<= &applied |]

                mkGenerator_Consecutive _ [] = bug "must have at least one unnamed type"
                mkGenerator_Consecutive perms [(u, uSize)] = do
                    (iPat, i) <- quantifiedVar
                    let perm = [essence| permutation((&i, succ(&i))) |]
                    let applied = combinedPermApply (perm:perms)
                    return [essence|
                                and([ &applied
                                    | &iPat : &u
                                    , &i < &uSize
                                    ])
                           |]
                mkGenerator_Consecutive perms ((u, uSize):us) = do
                    (iPat, i) <- quantifiedVar
                    let perm = [essence| permutation((&i, succ(&i))) |]
                    applied <- mkGenerator_Consecutive (perm:perms) us
                    return [essence|
                                and([ &applied
                                    | &iPat : &u
                                    , &i < &uSize
                                    ])
                           |]


                mkGenerator_AllPairs _ [] = bug "must have at least one unnamed type"
                mkGenerator_AllPairs perms [(u, _uSize)] = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    let perm = [essence| permutation((&i, &j)) |]
                    let applied = combinedPermApply (perm:perms)
                    return [essence|
                                and([ &applied
                                    | &iPat : &u
                                    , &jPat : &u
                                    , &i < &j
                                    ])
                           |]
                mkGenerator_AllPairs perms ((u, _uSize):us) = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    let perm = [essence| permutation((&i, &j)) |]
                    applied <- mkGenerator_AllPairs (perm:perms) us
                    return [essence|
                                and([ &applied
                                    | &iPat : &u
                                    , &jPat : &u
                                    , &i < &j
                                    ])
                           |]

                mkGenerator_AllPermutations _ [] = bug "must have at least one unnamed type"
                mkGenerator_AllPermutations perms [(u, _uSize)] = do
                    (iPat, i) <- quantifiedVar
                    let perm = i
                    let applied = combinedPermApply (perm:perms)
                    return [essence|
                                and([ &applied
                                    | &iPat : permutation of &u
                                    ])
                           |]
                mkGenerator_AllPermutations perms ((u, _uSize):us) = do
                    (iPat, i) <- quantifiedVar
                    let perm = i
                    applied <- mkGenerator_AllPermutations (perm:perms) us
                    return [essence|
                                and([ &applied
                                    | &iPat : permutation of &u
                                    ])
                           |]

                mkGenerator perms us =
                    case usbScope of
                        USBConsecutive -> mkGenerator_Consecutive perms us
                        USBAllPairs -> mkGenerator_AllPairs perms us
                        USBAllPermutations -> mkGenerator_AllPermutations perms us
            newCons <-
                case independentlyOrAltogether of
                    USBIndependently -> do
                      xs <- sequence
                            [ mkGenerator [] [(u, uSize)]
                            | (u@DomainReference{}, uSize) <- allUnnamedTypes
                            ]
                      return [SuchThat xs]
                    USBAltogether -> do
                        cons <- mkGenerator [] allUnnamedTypes
                        return [SuchThat [cons]]

            let stmts = newCons
            traceM $ show $ vcat $ "Adding the following unnamed symmetry breaking constraints:"
                                 : map (nest 4 . pretty) stmts
            return model { mStatements = mStatements model ++ stmts}



rule_Comprehension_Cardinality :: Rule
rule_Comprehension_Cardinality = "comprehension-cardinality" `namedRule` theRule where
    theRule p = do
        Comprehension _ gensOrConds <- match opTwoBars p
        let ofones = Comprehension (fromInt 1) gensOrConds
        return ( "Horizontal rule for comprehension cardinality"
               , return [essence| sum(&ofones) |]
               )

rule_Flatten_Cardinality :: Rule
rule_Flatten_Cardinality = "flatten-cardinality" `namedRule` theRule where
    theRule p = do
        list <- match opTwoBars p >>= match opConcatenate
        return ( "Horizontal rule for comprehension cardinality"
               , do
                   (iPat, i) <- quantifiedVar
                   return [essence| sum([ |&i| | &iPat <- &list ]) |]
               )

