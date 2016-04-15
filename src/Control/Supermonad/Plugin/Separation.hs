
-- | Functions to separate a set of wanted constrains into groups of 
--   constraints that require being solved together.
module Control.Supermonad.Plugin.Separation 
  ( separateContraints
  , componentTopTyCons
  , componentTopTcVars
  , componentMonoTyCon
  ) where

import Data.Maybe ( fromJust )
import qualified Data.Set as S

import Control.Monad ( filterM, liftM2 )

import Data.Graph.Inductive.Graph 
  ( LNode, Edge
  , mkGraph, toLEdge )
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.Graph.Inductive.Query.DFS ( components )

import TcRnTypes ( Ct )
import TyCon ( TyCon )
import Type ( Type, TyVar )
import TcType ( isAmbiguousTyVar )

import Control.Supermonad.Plugin.Constraint
  ( WantedCt, constraintClassTyArgs )
import Control.Supermonad.Plugin.Utils
  ( collectTopTyCons, collectTopTcVars, anyM )
import Control.Supermonad.Plugin.Environment
  (  SupermonadPluginM )
import Control.Supermonad.Plugin.Environment.Lift
  ( isReturnConstraint, isBindConstraint )

type SCNode = LNode WantedCt

-- | Checks if the given component only involved exactly one top-level type constructor
--   in its supermonad constraints.
componentMonoTyCon :: [WantedCt] -> SupermonadPluginM (Maybe TyCon)
componentMonoTyCon cts = do
  -- Find all of the return and bind constraints
  smCts <- filterM (\ct -> liftM2 (||) (isReturnConstraint ct) (isBindConstraint ct)) cts
  -- Get the polymorphic type constructors
  let tyVars = S.filter (not . isAmbiguousTyVar) $ componentTopTcVars smCts
  -- Get the concrete type constructors
  let tyCons = componentTopTyCons smCts
  return $ case (S.toList tyCons, S.size tyVars) of
    ([tc], 0) -> Just tc
    _ -> Nothing

-- | Collect all top-level type constructors for the given list of 
--   wanted constraints. See 'collectTopTyCons'.
componentTopTyCons :: [WantedCt] -> S.Set TyCon
componentTopTyCons = collect collectTopTyCons

-- | Collect all top-level type constructors variables for the given list of 
--   wanted constraints. See 'collectTopTyCons'.
componentTopTcVars :: [WantedCt] -> S.Set TyVar
componentTopTcVars = collect collectTopTcVars

-- | Utility function that applies the given collection functions to all
--   type arguments of the given constraints and returns a list of the
--   collected results. Duplicates are removed from the result list.
collect :: (Ord a) => ([Type] -> S.Set a) -> [Ct] -> S.Set a
collect f cts = S.unions $ fmap collectLocal cts
  where 
    -- collectLocal :: WantedCt -> S.Set a
    collectLocal ct = maybe S.empty id 
                    $ fmap f
                    $ constraintClassTyArgs ct

-- | Creates a graph of the constraints and how they are 
--   conntected by their top-level ambiguous type constructor variables. 
--   Returns the connected components of that graph. 
--   These components represent the groups of constraints that are in need of 
--   solving and have to be handeled together.
separateContraints :: [WantedCt] -> SupermonadPluginM [[WantedCt]]
separateContraints wantedCts = filterM containsBindOrReturn comps
  where
    -- | Checks if the given constraint group contains any 'Bind' or 'Return'
    --   constraints.
    containsBindOrReturn :: [WantedCt] -> SupermonadPluginM Bool
    containsBindOrReturn = anyM $ \ct -> liftM2 (||) (isBindConstraint ct) (isReturnConstraint ct)
    
    comps :: [[WantedCt]]
    comps = fmap (\n -> fromJust $ lookup n nodes) <$> components g
    
    g :: Gr WantedCt ()
    g = mkGraph nodes (fmap (\e -> toLEdge e ()) edges)
    
    -- | Each constraint is a node.
    nodes :: [SCNode]
    nodes = zip [0..] wantedCts
    
    -- | An edge between two constraints exists when they have a common 
    --   top-level type constructor or type constructor variables in their 
    --   type arguments.
    edges :: [Edge]
    edges = [ e | e <- allEdgesFor nodes, isEdge e ]
    
    -- | Returns 'True' if the given edge is an edge of the graph.
    isEdge :: Edge -> Bool
    isEdge (na, nb) = maybe False id $ do
      -- Lookup the constraints associated with the nodes in the given
      -- edge and keep their type arguments.
      caArgs <- lookup na nodes >>= constraintClassTyArgs
      cbArgs <- lookup nb nodes >>= constraintClassTyArgs
      -- Collect all top level type constructors and type constructor variables
      -- in the type arguments.
      let ta = S.filter isAmbiguousTyVar $ collectTopTcVars caArgs
      let tb = S.filter isAmbiguousTyVar $ collectTopTcVars cbArgs
      -- If there is an element in the intersection of these sets 
      return $ not $ S.null $ S.intersection ta tb
    
    -- | Returns the edges for a complete undirected graph of the given nodes.
    allEdgesFor :: [SCNode] -> [Edge]
    allEdgesFor [] = []
    allEdgesFor (n : ns) = concat [ fmap (\m -> (m, fst n)) (fmap fst ns)
                                  , fmap (\m -> (fst n, m)) (fmap fst ns)
                                  , allEdgesFor ns ]