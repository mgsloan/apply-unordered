{-# LANGUAGE LambdaCase #-}

module Control.Apply.Unordered.Plugin where

import Control.Monad (guard)
import Data.Generics (everything, mkQ)
import Data.Traversable (for)
import GHC.TcPluginM.Extra (lookupModule, lookupName, evByFiat)
import Data.Maybe

import GhcPlugins
import TcEvidence (EvTerm (..))
import TcPluginM (TcPluginM, tcLookupTyCon, newGiven)
import TcRnTypes
import TcType (isTyFamFree)
import Unify (tcMatchTy)
import TyCoRep

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const $ Just $ TcPlugin
      { tcPluginInit = initContext
      , tcPluginSolve = \context _ _ ->
          fmap (TcPluginOk []) . solveWanteds (implementTyFam context) (familyTyCon context)
      , tcPluginStop = const $ pure ()
      }
  , pluginRecompile = const $ pure NoForceRecompile
  }

data Context = Context
  { familyTyCon :: TyCon
  , typeErrorTyCon :: TyCon
  }

initContext :: TcPluginM Context
initContext = Context
  <$> loadTyCon
        "apply-unordered"
        "Control.Apply.Unordered"
        "BestParamIxImpl"
  <*> loadTyCon
        "base"
        "GHC.TypeLits"
        "TypeError"

implementTyFam :: Context -> [Type] -> Maybe Type
implementTyFam context [a, b] = do
  -- TODO: Are these guards really necessary? Useful to be able to
  -- ignore stuckness?
  guard $ isTyFamFree a
  guard $ isTyFamFree b
  pure $ promoteBestMatchResult context $ findBestMatch a b

promoteBestMatchResult :: Context -> BestMatchResult -> Type
promoteBestMatchResult context = \case
  BestMatch n -> LitTy (NumTyLit n)
  -- FIXME: Emit TypeError instead
  AmbiguousMatch _ -> error "Ambiguous"
  NoMatch -> error "No match"

data BestMatchResult
  = BestMatch Integer
  | AmbiguousMatch [(Integer, Type)]
  | NoMatch

findBestMatch :: Type -> Type -> BestMatchResult
findBestMatch arg fun =
  case foldr addMatch [] (candidateMatches arg 0 fun) of
    [] -> NoMatch
    [(ix, _)] -> BestMatch ix
    matches -> AmbiguousMatch matches

-- This logic was directly inspired by the function insert_overlapping
-- in ghc/compiler/types/InstEnv.hs
addMatch :: (Integer, Type) -> [(Integer, Type)] -> [(Integer, Type)]
addMatch new_item [] = [new_item]
addMatch new_item (old_item : old_items)
  -- New strictly overrides old
  | new_beats_old
  , not old_beats_new
  = addMatch new_item old_items

  -- Old strictly overrides new
  | old_beats_new
  , not new_beats_old
  = old_item : old_items

  | otherwise
  = old_item : addMatch new_item old_items
  where
    new_beats_old = snd new_item `more_specific_than` snd old_item
    old_beats_new = snd old_item `more_specific_than` snd new_item

    -- | l can be instantiated to match r, or they are equal
    more_specific_than :: Type -> Type -> Bool
    more_specific_than l r = isJust (tcMatchTy r l)

candidateMatches :: Type -> Integer -> Type -> [(Integer, Type)]
candidateMatches arg ix = \case
  FunTy paramTy resultTy
    | isJust (tcMatchTy paramTy arg) ->
        (ix, paramTy) : candidateMatches arg (ix + 1) resultTy
    | otherwise ->
        candidateMatches arg (ix + 1) resultTy

  -- TODO: Does GHC ever represent function types using TyConApp?
  TyConApp{} -> []

  -- TODO: Handle forall.
  ForAllTy{} -> []

  CastTy ty _ -> candidateMatches arg ix ty
  TyVarTy{} -> []
  AppTy{} -> []
  CoercionTy{} -> []

-- | Get your hands on the type constructor for your type family.
loadTyCon
    :: String  -- ^ package
    -> String  -- ^ module
    -> String  -- ^ identifier
    -> TcPluginM TyCon
loadTyCon p m i = do
  md <- lookupModule (mkModuleName m) $ fsLit p
  nm <- lookupName md $ mkTcOcc i
  tcLookupTyCon nm

-- | @solveWanteds f tyfam cts@ finds all instaces of @tyfam@ inside the wanted
-- constraints @cts@, and evaluates them via @f@. The result is a set of
-- 'CNonCanonical' constraints, which should be emitted as the second parameter
-- of 'TcPluginOk'.
solveWanteds
  :: ([Type] -> Maybe Type)
  -> TyCon
  -> [Ct]
  -> TcPluginM [Ct]
solveWanteds _ _ [] = pure []
solveWanteds f familyTyCon wanteds = do
  let rel = map (\wanted -> findRelevant f familyTyCon (ctLoc wanted) (ctev_pred (cc_ev wanted))) wanteds

  gs <- for (concat rel) $ \(MagicTyFamResult loc t res) -> do
    let EvExpr ev = evByFiat "magic-tyfams" t res
    newGiven loc (mkPrimEqPred t res) ev

  pure $ fmap CNonCanonical gs

-- | Locate and expand the use of any type families.
findRelevant :: ([Type] -> Maybe Type) -> TyCon -> CtLoc -> Type -> [MagicTyFamResult]
findRelevant f familyTyCon loc = everything (++) $ mkQ [] findCmpType
  where
    findCmpType t =
      case splitTyConApp_maybe t of
        Just (tc, ts) | tc == familyTyCon ->
           maybe [] (pure . MagicTyFamResult loc t) $ f ts
        _ -> []

data MagicTyFamResult = MagicTyFamResult
  { _mtfrLoc      :: CtLoc
  , _mtfrOriginal :: Type
  , _mtfrSolved   :: Type
  }
