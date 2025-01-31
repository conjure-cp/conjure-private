{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Conjure.Language.NameResolution
    ( resolveNames
    , resolveNamesMulti
    , resolveNamesX
    , resolveX, resolveD -- actually internal, use with care
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Domain
    ( changeRepr,
      typeOfDomain,
      Domain(DomainUnnamed, DomainReference, DomainRecord,
             DomainVariant) )
import Conjure.Language.Constant
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.TH



resolveNamesMulti ::
    MonadLog m =>
    MonadUserError m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    [Model] -> m [Model]
resolveNamesMulti = flip evalStateT [] . go
    where
        go [] = return []
        go (m:ms) = (:) <$> resolveNames_ m <*> go ms

resolveNames ::
    MonadLog m =>
    MonadUserError m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m Model
resolveNames = flip evalStateT [] . resolveNames_

resolveNames_ ::
    MonadLog m =>
    MonadState [(Name, ReferenceTo)] m =>
    MonadUserError m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m Model
resolveNames_ model = failToUserError $ do
    statements <- mapM resolveStatement (mStatements model)
    mapM_ check (universeBi statements)
    return model { mStatements = toTaggedInt statements }

-- this is for when a name will shadow an already existing name that is outside of this expression
-- we rename the new names to avoid name shadowing
shadowing ::
    MonadFailDoc m =>
    MonadState [(Name, ReferenceTo)] m =>
    NameGen m =>
    Expression -> m Expression
shadowing p@(Comprehension _ is) = do
    -- list of names originating from this comprehension
    let generators = concat
            [ names
            | Generator gen <- is
            , let pat = generatorPat gen
            , let names = [ n | n@Name{} <- universeBi pat ]
            ]
    ctxt <- gets id
    -- a subset of names originating from this comprehension that will shadow already existing names
    let shadows = [ g | g <- generators, g `elem` map fst ctxt ]
    shadowsNew <- forM shadows $ \ s -> do n <- nextName "shadow" ; return (s,n)
    let f n = fromMaybe n (lookup n shadowsNew)
    return (transformBi f p)
shadowing p = return p


addName ::
    MonadState [(Name, ReferenceTo)] m =>
    MonadUserError m =>
    Name -> ReferenceTo -> m ()
addName n thing = do
    ctxt <- gets id
    let
        allowed (DeclNoRepr _ _ _ _) (Alias _) = True -- needed when instantiating stuff
        allowed (DeclHasRepr _ _ _) (Alias _) = True -- needed when instantiating stuff
        allowed old new = old == new
    let mdefined = [ thing' | (n', thing') <- ctxt, n == n' && not (allowed thing' thing) ]
    case mdefined of
        [] -> return ()
        (thing':_) -> userErr1 $ vcat [ "Redefinition of name:" <+> pretty n
                                      , "When trying to define it as" <+> pretty thing
                                      , "It was already defined as" <+> pretty thing'
                                      ]
    modify ((n, thing) :)


resolveNamesX ::
    MonadFailDoc m =>
    MonadUserError m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Expression -> m Expression
resolveNamesX x = do
    x' <- evalStateT (resolveX x) []
    mapM_ check (universe x')
    return x'


toTaggedInt :: Data a => a -> a
toTaggedInt = transformBi f
    where
        f :: Type -> Type
        f (TypeEnum (Name nm)) = TypeInt (TagEnum nm)
        f ty = ty


check :: MonadFailDoc m => Expression -> m ()
check (Reference nm Nothing) = failDoc ("Undefined:" <+> pretty nm)
check _ = return ()


resolveStatement ::
    MonadFailDoc m =>
    MonadState [(Name, ReferenceTo)] m =>
    MonadUserError m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Statement -> m Statement
resolveStatement st =
    case st of
        Declaration decl ->
            case decl of
                FindOrGiven forg nm dom       -> do
                    dom' <- resolveD dom
                    addName nm $ DeclNoRepr forg nm dom' NoRegion
                    return (Declaration (FindOrGiven forg nm dom'))
                Letting nm x                  -> do
                    x' <- resolveX x
                    addName nm $ Alias x'
                    return (Declaration (Letting nm x'))
                LettingDomainDefnUnnamed nm x -> do
                    x' <- resolveX x
                    addName nm $ Alias (Domain (DomainUnnamed nm x'))
                    return (Declaration (LettingDomainDefnUnnamed nm x'))
                LettingDomainDefnEnum (Name ename) nms -> do
                    sequence_ [ addName nm $ Alias (Constant (ConstantInt (TagEnum ename) i))
                              | (nm, i) <- zip nms [1..]
                              ]
                    return st
                LettingDomainDefnEnum{} -> bug "resolveStatement, Name"
                GivenDomainDefnEnum{}       -> return st             -- ignoring
        SearchOrder xs -> SearchOrder <$> mapM resolveSearchOrder xs
        SearchHeuristic nm -> do
            let allowed = ["static", "sdf", "conflict", "srf", "ldf", "wdeg", "domoverwdeg"]
            if nm `elem` allowed
                then return (SearchHeuristic nm)
                else userErr1 $ vcat [ "Invalid heuristic:" <+> pretty nm
                                     , "Allowed values are:" <+> prettyList id "," allowed
                                     ]
        Where xs -> Where <$> mapM resolveX xs
        Objective obj x -> Objective obj <$> resolveX x
        SuchThat xs -> SuchThat <$> mapM resolveX xs


resolveSearchOrder ::
    MonadFailDoc m =>
    MonadState [(Name, ReferenceTo)] m =>
    MonadUserError m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    SearchOrder -> m SearchOrder
resolveSearchOrder (BranchingOn nm) = do
    ctxt <- gets id
    mval <- gets (lookup nm)
    case mval of
        Nothing -> userErr1 $ vcat $ ("Undefined reference:" <+> pretty nm)
                                   : ("Bindings in context:" : prettyContext ctxt)
        Just{}  -> return (BranchingOn nm)
resolveSearchOrder (Cut x) =
    let f Find = CutFind
        f forg = forg
    in  Cut . transformBi f <$> resolveX x


resolveX ::
    MonadFailDoc m =>
    MonadState [(Name, ReferenceTo)] m =>
    MonadUserError m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Expression -> m Expression
resolveX (Reference nm Nothing) = do
    ctxt <- gets id
    mval <- gets (lookup nm)
    case mval of
        Nothing -> userErr1 $ vcat $ ("Undefined reference:" <+> pretty nm)
                                   : ("Bindings in context:" : prettyContext ctxt)
        Just r  -> return (Reference nm (Just r))

resolveX p@(Reference nm (Just refto)) = do             -- this is for re-resolving
    mval <- gets (lookup nm)
    case mval of
        Nothing -> return p                             -- hence, do not failDoc if not in the context
        Just DeclNoRepr{}                               -- if the newly found guy doesn't have a repr
            | DeclHasRepr{} <- refto                    -- but the old one did, do not update
            -> return p
        Just (DeclNoRepr forg_ nm_ dom_ _)              -- if the newly found guy doesn't have a repr
            | DeclNoRepr _ _ _ region <- refto          -- and the old one didn't have one either
                                                        -- preserve the region information
            -> return (Reference nm (Just (DeclNoRepr forg_ nm_ dom_ region)))
        Just (Alias r) -> do
            r' <- resolveX r
            return (Reference nm (Just (Alias r')))
        Just r ->
            return (Reference nm (Just r))

resolveX (AbstractLiteral lit) = AbstractLiteral <$> resolveAbsLit lit

resolveX (Domain x) = Domain <$> resolveD x

resolveX p@Comprehension{} = scope $ do
    p' <- shadowing p
    case p' of
        Comprehension x is -> do
            is' <- forM is $ \ i -> case i of
                Generator gen -> do
                    (gen', refto) <- case gen of
                        GenDomainNoRepr pat dom -> do
                            dom' <- resolveD dom
                            let gen'' = GenDomainNoRepr pat dom'
                            return
                                ( gen''
                                , case pat of
                                    Single nm' -> DeclNoRepr Quantified nm' dom' NoRegion
                                    _ -> InComprehension gen''
                                )
                        GenDomainHasRepr nm dom -> do
                            dom' <- resolveD dom
                            return
                                ( GenDomainHasRepr nm dom'
                                , DeclHasRepr Quantified nm dom'
                                )
                        GenInExpr pat expr -> do
                            expr' <- resolveX expr
                            let gen'' = GenInExpr pat expr'
                            return ( gen'' , InComprehension gen'' )
                    forM_ (universeBi (generatorPat gen)) $ \ nm ->
                        addName nm refto
                    return (Generator gen')
                Condition y -> Condition <$> resolveX y
                ComprehensionLetting pat expr -> do
                    expr' <- resolveX expr
                    resolveAbsPat p pat expr'
                    return (ComprehensionLetting pat expr')
            x' <- resolveX x
            return (Comprehension x' is')
        _ -> bug "NameResolution.resolveX.shadowing"

resolveX (WithLocals body (AuxiliaryVars locals)) = scope $ do
    locals' <- mapM resolveStatement locals
    body'   <- resolveX body
    return (WithLocals body' (AuxiliaryVars locals'))

resolveX (WithLocals body (DefinednessConstraints locals)) = scope $ do
    locals' <- mapM resolveX locals
    body'   <- resolveX body
    return (WithLocals body' (DefinednessConstraints locals'))

resolveX x = descendM resolveX x


resolveD ::
    MonadFailDoc m =>
    MonadState [(Name, ReferenceTo)] m =>
    MonadUserError m =>
    NameGen m =>
    Data r =>
    Default r =>
    Pretty r =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Domain r Expression -> m (Domain r Expression)
resolveD (DomainReference nm (Just d)) = DomainReference nm . Just <$> resolveD d
resolveD (DomainReference nm Nothing) = do
    mval <- gets (lookup nm)
    case mval of
        Nothing -> userErr1 ("Undefined reference to a domain:" <+> pretty nm)
        Just (Alias (Domain r)) -> DomainReference nm . Just <$> resolveD (changeRepr def r)
        Just x -> userErr1 ("Expected a domain, but got an expression:" <+> pretty x)
resolveD (DomainRecord ds) = fmap DomainRecord $ forM ds $ \ (n, d) -> do
    d' <- resolveD d
    t  <- typeOfDomain d'
    addName n $ RecordField n t
    return (n, d')
resolveD (DomainVariant ds) = fmap DomainVariant $ forM ds $ \ (n, d) -> do
    d' <- resolveD d
    t  <- typeOfDomain d'
    addName n $ VariantField n t
    return (n, d')
resolveD d = do
    d' <- descendM resolveD d
    mapM resolveX d'


resolveAbsPat ::
    MonadState [(Name, ReferenceTo)] m =>
    MonadUserError m =>
    Expression -> AbstractPattern -> Expression -> m ()
resolveAbsPat _ AbstractPatternMetaVar{} _ = bug "resolveAbsPat AbstractPatternMetaVar"
resolveAbsPat _ (Single nm) x = addName nm $ Alias x
resolveAbsPat context (AbsPatTuple ps) x =
    sequence_ [ resolveAbsPat context p [essence| &x[&i] |]
              | (p, i_) <- zip ps allNats
              , let i   = fromInt i_ 
              ]
resolveAbsPat context (AbsPatMatrix ps) x =
    sequence_ [ resolveAbsPat context p [essence| &x[&i] |]
              | (p, i_) <- zip ps allNats
              , let i  = fromInt i_
              ]
resolveAbsPat context (AbsPatSet ps) x = do
    ys <- case x of
        Constant (viewConstantSet -> Just xs) -> return (map Constant xs)
        AbstractLiteral (AbsLitSet xs) -> return xs
        _ -> userErr1 $ "Abstract set pattern cannot be used in this context:" <++> pretty context
    sequence_ [ resolveAbsPat context p y
              | (p,y) <- zip ps ys
              ]


resolveAbsLit ::
    MonadFailDoc m =>
    MonadState [(Name, ReferenceTo)] m =>
    MonadUserError m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    AbstractLiteral Expression -> m (AbstractLiteral Expression)
resolveAbsLit (AbsLitVariant Nothing n x) = do
    x'   <- resolveX x
    mval <- gets id
    let
        isTheVariant (Alias (Domain d@(DomainVariant nms))) | Just{} <- lookup n nms = Just d
        isTheVariant _ = Nothing
    case mapMaybe (isTheVariant . snd) mval of
        (DomainVariant dom:_) -> return (AbsLitVariant (Just dom) n x')
        _ -> return (AbsLitVariant Nothing n x')
resolveAbsLit lit = (descendBiM resolveX >=> descendBiM resolveD') lit
    where
        resolveD' d = resolveD (d :: Domain () Expression)
