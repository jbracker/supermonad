
-- | Functions to separate a set of wanted constrains into groups of 
--   constraints that require being solved together.
module Control.Super.Plugin.Separation 
  ( ConstraintGroup
  , separateContraints
  , componentTopTyCons
  , componentTopTcVars
  , componentMonoTyCon
  ) where

import Data.Maybe ( fromJust, fromMaybe )
import qualified Data.Set as Set

import Data.Graph.Inductive.Graph 
  ( LNode, Edge
  , mkGraph, toLEdge )
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.Graph.Inductive.Query.DFS ( components )

import TcRnTypes ( Ct )
import TyCon ( TyCon )
import Type ( Type, TyVar )
import TcType ( isAmbiguousTyVar )
import Class ( Class )
--import Unique ( Uniquable )

import qualified Control.Super.Plugin.Collection.Set as S
import Control.Super.Plugin.Constraint
  ( WantedCt
  , constraintClassTyArgs
  , isAnyClassConstraint )
import Control.Super.Plugin.Utils
  ( collectTopTyCons, collectTopTcVars )

type SCNode = LNode WantedCt

-- | A group of constraints that is connected by common ambiguous type variables.
type ConstraintGroup = [WantedCt]

-- | Checks if the given component only involved exactly one top-level type constructor
--   in its supermonad constraints. Which classes make up the supermonad constraints
--   is given by the list of classes.
componentMonoTyCon :: [Class] -> ConstraintGroup -> Maybe TyCon
componentMonoTyCon releventClss cts = 
  let -- Find all of the return and bind constraints
      smCts = filter (isAnyClassConstraint releventClss) cts
      -- Get the polymorphic type constructors
      tyVars = Set.filter (not . isAmbiguousTyVar) $ componentTopTcVars smCts
      -- Get the concrete type constructors
      tyCons = componentTopTyCons smCts
  in case (S.toList tyCons, Set.size tyVars) of
    ([tc], 0) -> Just tc
    _ -> Nothing

-- | Collect all top-level type constructors for the given list of 
--   wanted constraints. See 'collectTopTyCons'.
componentTopTyCons :: [WantedCt] -> S.Set TyCon
componentTopTyCons = collectInternalSet collectTopTyCons

-- | Collect all top-level type constructors variables for the given list of 
--   wanted constraints. See 'collectTopTyCons'.
componentTopTcVars :: [WantedCt] -> Set.Set TyVar
componentTopTcVars = collect collectTopTcVars

-- | Utility function that applies the given collection functions to all
--   type arguments of the given constraints and returns a list of the
--   collected results. Duplicates are removed from the result list.
collect :: (Ord a) => ([Type] -> Set.Set a) -> [Ct] -> Set.Set a
collect f cts = Set.unions $ fmap collectLocal cts
  where 
    -- collectLocal :: WantedCt -> S.Set a
    collectLocal ct = maybe Set.empty f $ constraintClassTyArgs ct

collectInternalSet :: ([Type] -> S.Set a) -> [Ct] -> S.Set a
collectInternalSet f cts = S.unions $ fmap collectLocal cts
  where 
    -- collectLocal :: WantedCt -> S.Set a
    collectLocal ct = maybe S.empty f $ constraintClassTyArgs ct

-- | Creates a graph of the constraints and how they are 
--   conntected by their top-level ambiguous type constructor variables. 
--   Returns the connected components of that graph. 
--   These components represent the groups of constraints that are in need of 
--   solving and have to be handeled together.
separateContraints :: [WantedCt] -> [ConstraintGroup]
separateContraints wantedCts = comps
  where
    comps :: [ConstraintGroup]
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
    isEdge (na, nb) = fromMaybe False $ do
      -- Lookup the constraints associated with the nodes in the given
      -- edge and keep their type arguments.
      caArgs <- lookup na nodes >>= constraintClassTyArgs
      cbArgs <- lookup nb nodes >>= constraintClassTyArgs
      -- Collect all top level type constructors and type constructor variables
      -- in the type arguments.
      let ta = Set.filter isAmbiguousTyVar $ collectTopTcVars caArgs
      let tb = Set.filter isAmbiguousTyVar $ collectTopTcVars cbArgs
      -- If there is an element in the intersection of these sets 
      return $ not $ Set.null $ Set.intersection ta tb
    
    -- | Returns the edges for a complete undirected graph of the given nodes.
    allEdgesFor :: [SCNode] -> [Edge]
    allEdgesFor [] = []
    allEdgesFor (n : ns) = concat [ fmap (\m -> (fst m, fst n)) ns
                                  , fmap (\m -> (fst n, fst m)) ns
                                  , allEdgesFor ns ]