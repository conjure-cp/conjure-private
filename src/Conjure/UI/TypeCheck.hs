{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Conjure.UI.TypeCheck ( typeCheckModel_StandAlone, typeCheckModel ) where

-- conjure
import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.CategoryOf ( categoryChecking )
import Conjure.Language.Pretty
import Conjure.Language.Lenses
import Conjure.Language.TH ( essence )
import Conjure.Process.Enums ( removeEnumsFromModel )
import Conjure.Process.Unnameds ( removeUnnamedsFromModel )
import Conjure.Language.NameResolution ( resolveNames )
import Conjure.Process.Sanity ( sanityChecks )



typeCheckModel_StandAlone ::
    MonadFailDoc m =>
    MonadUserError m =>
    MonadLog m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m Model
typeCheckModel_StandAlone model0 = do
    -- for better error messages, type-check and category-check before sanity-checking.
    -- sanity checking will modify the model.
    -- then, type-check once more just in case the newly generated
    -- stuff is broken.
    model1 <- return model0             >>= logDebugId "[input]"
          >>= removeUnnamedsFromModel   >>= logDebugId "[removeUnnamedsFromModel]"
          >>= removeEnumsFromModel      >>= logDebugId "[removeEnumsFromModel]"
          >>= resolveNames              >>= logDebugId "[resolveNames]"
          >>= typeCheckModel            >>= logDebugId "[typeCheckModel]"
          >>= categoryChecking          >>= logDebugId "[categoryChecking]"
          >>= sanityChecks              >>= logDebugId "[sanityChecks]"
          >>= typeCheckModel            >>= logDebugId "[typeCheckModel]"
    return model1


typeCheckModel ::
    MonadFail m =>
    MonadUserError m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m Model
typeCheckModel model1 = do
    let model2 = fixRelationProj model1
    (statements3, errs) <- runWriterT $ forM (mStatements model2) $ \ st ->
        case st of
            Declaration decl -> do
                case decl of
                    FindOrGiven _ _ domain ->
                        case domain of
                            DomainReference{} ->
                                -- no need to raise an error here if this is a reference to another domain
                                -- because must have already we raised the error in the "letting" of that domain
                                return ()
                            _ -> do
                                mty <- runExceptT $ typeOfDomain domain
                                case mty of
                                    Right _ -> return ()
                                    Left err -> tell $ return $ vcat
                                        [ "In a declaration statement:" <++> pretty st
                                        , "Error:" <++> pretty err
                                        ]
                    Letting _ x -> do
                        mty <- runExceptT $ case x of
                                                Domain y -> typeOfDomain y
                                                _ -> typeOf x
                        case mty of
                            Right _ -> return ()
                            Left err -> tell $ return $ vcat
                                [ "In a letting statement:" <++> pretty st
                                , "Error:" <++> pretty err
                                ]
                    GivenDomainDefnEnum{} -> return ()
                    LettingDomainDefnEnum{} -> return ()
                    LettingDomainDefnUnnamed _ x -> do
                        mty <- runExceptT $ typeOf x
                        case mty of
                            Right TypeInt{} -> return ()
                            Left err -> tell $ return $ vcat
                                [ "In the declaration of an unnamed type:" <++> pretty st
                                , "Error:" <++> pretty err
                                ]
                            Right ty -> tell $ return $ vcat
                                [ "In the declaration of an unnamed type:" <++> pretty st
                                , "Expected type `int`, but got:" <++> pretty ty
                                ]
                return st
            SearchOrder xs -> do
                forM_ xs $ \case
                    BranchingOn{} -> return ()                          -- TODO: check if the name is defined
                    Cut x -> do
                        mty <- runExceptT $ typeOf x
                        case mty of
                            Right TypeBool{} -> return ()
                            Left err -> tell $ return $ vcat
                                [ "In a 'branching on' statement:" <++> pretty x
                                , "Error:" <++> pretty err
                                ]
                            Right ty -> tell $ return $ vcat
                                [ "In a 'branching on' statement:" <++> pretty x
                                , "Expected type `bool`, but got:" <++> pretty ty
                                ]
                return st
            SearchHeuristic{} -> return st
            Where xs -> do
                xs' <- forM xs $ \ x -> do
                    mty <- runExceptT $ typeOf x
                    case mty of
                        Right TypeBool{} -> return x
                        Right (TypeList TypeBool) -> return (make opAnd x)
                        Right (TypeMatrix _ TypeBool) -> return (make opAnd x)
                        Left err -> do
                            tell $ return $ vcat
                                [ "In a 'where' statement:" <++> pretty x
                                , "Error:" <++> pretty err
                                ]
                            return x
                        Right ty -> do
                            tell $ return $ vcat
                                [ "In a 'where' statement:" <++> pretty x
                                , "Expected type `bool`, but got:" <++> pretty ty
                                ]
                            return x
                return (Where xs')
            Objective minMax x -> do
                mty <- runExceptT $ typeOf x
                let
                    go TypeInt{} o = return o
                    go (TypeTuple ts) o =
                        fromList <$> sequence [ go t [essence| &o[&i] |]
                                              | (i', t) <- zip allNats ts
                                              , let i :: Expression = fromInt i'
                                              ]
                    go (TypeMatrix _ t) (AbstractLiteral (AbsLitMatrix _ os)) =
                        fromList <$> sequence [ go t o | o <- os ]
                    go t o = do
                        tell $ return $ vcat
                            [ "In the objective:" <++> pretty st
                            , "Expected type `int`, but got:" <++> pretty t
                            , "Expression:" <+> pretty o
                            ]
                        return o
                case mty of
                    Right ty -> do
                        x' <- go ty x
                        return (Objective minMax x')
                    Left err -> do
                        tell $ return $ vcat
                            [ "In the objective:" <++> pretty st
                            , "Error:" <++> pretty err
                            ]
                        return st
            SuchThat xs -> do
                xs' <- forM xs $ \ x -> do
                    mty <- runExceptT $ typeOf x
                    case mty of
                        Right TypeBool{} -> return x
                        Right (TypeList TypeBool) -> return (make opAnd x)
                        Right (TypeMatrix _ TypeBool) -> return (make opAnd x)
                        Left err -> do
                            tell $ return $ vcat
                                [ "In a 'such that' statement:" <++> pretty x
                                , "Error:" <++> pretty err
                                ]
                            return x
                        Right ty -> do
                            tell $ return $ vcat
                                [ "In a 'such that' statement:" <++> pretty x
                                , "Expected type `bool`, but got:" <++> pretty ty
                                ]
                            return x
                return (SuchThat xs')
    unless (null errs) (userErr errs)

    -- now that everything knows its type, we can recover
    -- DomainInt [RangeSingle x] from DomainIntE x, if x has type int
    let
        domainIntERecover :: forall m . MonadFailDoc m => Domain () Expression -> m (Domain () Expression)
        domainIntERecover d@(DomainIntE maybe_tag x) = do
            ty <- runExceptT $ typeOf x
            return $ case ty of
                Right (TypeInt _) -> DomainInt maybe_tag [RangeSingle x]
                _ -> d
        domainIntERecover d = return d
    statements4 <- transformBiM domainIntERecover statements3

    return model2 { mStatements = statements4 }

