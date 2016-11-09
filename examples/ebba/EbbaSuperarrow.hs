{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- TODO:
-- * Observations should really only be fed in once! And then
--   reused. Cannot change! But tricky. See discussion below.
-- 
-- * It would be nice to have a *static* characetrisation of
--   estimability. At present, unestimable models will be caught
--   only dynamically, albeit quickly, as soon as an "undefined"
--   hits a logpdf.
--
-- * Investigate how to express HasObs using type functions/associated
--   types instead of functional dependency. Current attempt leads
--   to ambigous type variables and hence failed overloading resolution.
--
-- NOTES:
-- Observation: the *observation* should not change during estimation.
-- So really should be integrated into the local state. But this
-- would require identifying and separating the estimates from the
-- observations which in turn would require partitioning the "fused"
-- variable vector and making part of it part of the state, and the
-- other coming from the estimates. Sounds tricky.
--
-- An alternative could be to "cache" the observation part and "re-fuse"
-- with that over and over. That should be do-able. Might require
-- a different repr. of the "running" computation (as the observation
-- part now is part of the state). And thus also composition
-- defined in terms of that representation.
--
-- First step: go through the code and ensure that the "outcome observations"
-- just are fed trough, and never contaminated by any estimates.
-- If so, ought to be possible to "cache" those.
--
-- No, that simplistic idea is NOT going to work. Fused data is fed into
-- the observed outcomes by >>>. Or consider a generator for parameters:
-- we don't know if that parameter ultimately was observed or not locally,
-- so how could we cache? Indeed, this may be different for different
-- instances!
--
-- Conceivably, as a kind of "compilation" (and maybe a precursor to
-- real compilation?), we could imagine the initialization phase
-- tagging estimates and observations to tell them apart. Maybe
-- one could then do a "dynamic optimization" that would achieve the
-- caching? (Recall ideas for "change propagation").
--
-- But the overheads probably outweigh the advantages unless we do real
-- compilation.
--
-- For compilation, consider some kind of "symbolic evaluation" with
-- (generated) variable names in place of the estimate, while actual
-- observatios are fed through? JIT!!!
--
-- Maybe some PHOAS ideas could be used? After all, we *are* using
-- the host-language functiosn for bindings.

-- THOUGHT
-- If the actual data propagated were wrapped in a type:
--
-- data Variable a = Estimate a | Observed a
--
-- that would be an interesting first step towards:
-- * Compilation
-- * Multi-dim. automatic differentiation.
--
-- For example, for compilation, the type might be:
--
-- data Variable a = Est "name" a a | Obs a
--
-- where we keep track of name, init. value, and init sd (or some
-- more general notion of perturbation if necessary, "Pertub a".
--
-- All this hinges on NO USE OF "arr" for routing.
--
-- Key thing: we need routing blcoks!
--
-- is it possible to compute: routing :: r -> Cp o a b
-- 
-- where r is a rearrangement shaped like b giving position in a
-- for each element of a.
--
-- More promising idea than fixed set of blocks for exchange,
-- reassoc. Note that we must NOT use polymorphism for routing!
-- Need to routhe at element level if an idea like wrapping
-- up data in extra info is to be used.
--
-- Note: for arrow like syntax, we obviously have a problem:
--
-- a <- XXX -<
--
--   <- YYY -< <| a, 1, y |>
--
-- "a" must be routed through untouched.
-- The others must be lifted (assume y is lambda bound).
-- 
-- But not that different from e.g. FHM: distinction between
-- signal variables and everything else.
--
-- One could imagine a basic, silent lifting, where little info
-- is known about the lifted entity, and "annotated liftings".
-- where more info is provided for injection into the boxes/wrapping the
-- lifted entities.
--
-- ANOTHER THOUGHT
-- Is it conceivable to use GHC rewrite rules to map "arr route"
-- to specific routing blocks instead??? As a quick way of making
-- use of existing arrow syntax infrastructure.
--
-- Did use it for "arr id = identity" in the past at least.


-- module Ebba where
module Main where

import Data.Maybe (fromJust)
import Data.List (unzip5)
import Control.Monad
import Control.Monad.Fix
import System.Random
import GHC.TypeLits
import qualified Data.Map as M	-- Try to import Data.Map.Strict instead.
				-- If that works, supposedly better.

-- Gnewplot
import Graphics.Gnewplot.Histogram
import Graphics.Gnewplot (gnuplotOnScreen, gnuplotToPS)

import qualified Control.Super.Arrow.Constrained as A

infixr 3 ***
infixr 3 &&&
infixr 1 >>>


------------------------------------------------------------------------------
-- Basic definitions
------------------------------------------------------------------------------

data Stream a = Cons a (Stream a)


uniformRandomS :: RandomGen g => g -> Stream Double
uniformRandomS g = Cons (1 - r) (uniformRandomS g')
    where
        (r, g') = random g


newtype Prob a = Prob {unProb :: Stream Double -> (a, Stream Double)}


instance Functor Prob where

    fmap f pa = Prob (\s -> let (a, s') = unProb pa s in (f a, s'))


instance Monad Prob where

    return a = Prob (\s -> (a, s))

    pa >>= f = Prob $ \s ->
                   let
                       (a, s') = unProb pa s 
                   in
                       unProb (f a) s'

instance Applicative Prob where
  mf <*> ma = mf >>= \f -> fmap f ma
  pure = return

instance MonadFix Prob where
    mfix f = Prob $ \s ->
                 let
                     as' = unProb (f (fst as')) s
                 in
                     as'


unit :: Prob Double
unit = Prob $ \(Cons r s) -> (r, s)


runProb :: RandomGen g => g -> Prob a -> a
runProb g pa = fst (unProb pa (uniformRandomS g))


------------------------------------------------------------------------------
-- Distributions
------------------------------------------------------------------------------


bernoulli :: Double -> Prob Bool
bernoulli p = do
    x <- unit
    return (x <= p)


lpdfBernoulli :: Double -> Bool -> Double
lpdfBernoulli p b
    | p < 0 || p > 1 = log 0	-- -infinity
    | otherwise      = case b of
                           False -> log (1 - p)
                           True  -> log p


uniform :: Double -> Double -> Prob Double
uniform a b = do
    x <- unit
    return (a + x * (b - a))


lpdfUniform :: Double -> Double -> Double -> Double
lpdfUniform a b x
    | b <= a         = error "Bad parameters uniform_pdf"
    | x < a || x > b = log 0	-- -infinity
    | otherwise      = - log (b - a)


-- Gaussian using the Box-Muller transform.

gaussian :: Double -> Double -> Prob Double
gaussian m sd = do
    u <- unit
    v <- unit
    return (m + sd * sqrt ((-2.0) * log u) * cos (2.0 * pi * v))


------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- Biased sample mean and variance. 
--
-- There are much more suitable on-line methods:
--
--   http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
--
-- But we don't really care for now: we're just using this to get
-- our markov chains started with a reasonbale spread for each
-- variable, and so we're only going to compute for small sample sets.

meanVar :: [Double] -> (Double, Double)
meanVar [] = error "Cannot compute mean and variance of empty set of samples."
meanVar xs = mvAux 0 0 0 xs
    where
        mvAux :: Int -> Double -> Double -> [Double] -> (Double, Double)
        mvAux n s ss [] = (mean, var)
            where
                n'   = fromIntegral n
                mean = s/n'
                var  = ss/n' - mean * mean
        mvAux n s ss (x:xs) = mvAux (n+1) (s + x) (ss + x * x) xs


------------------------------------------------------------------------------
-- Parameters
------------------------------------------------------------------------------

type Name = String

data ParVal = PVScalar Double		-- Maybe: PVVec2 (Double, Double) etc.
            deriving Show

class ParType a where
    toPV :: a -> ParVal


instance ParType Double where
    toPV x = PVScalar x


type Parameters = M.Map Name ParVal


------------------------------------------------------------------------------
-- Observability
------------------------------------------------------------------------------

class HasObs a o | a -> o where
    obs :: a -> o


{-
-- How to do it with without functional dependencies?
class HasObs a o where
    type Obs a :: *
    obs :: a -> Obs a
-}

data U			-- Unobservable
data O (p :: [Nat])	-- Observable from position p


type family o1 *** o2
type instance o1 *** o2 = (Prp 1 o1, Prp 2 o2)


-- Prepend an index i to an observation yielding an observation representing
-- a product of projection functions where each one is teh composition of
-- the projection function pi_i and the original projection function.

type family Prp (i :: Nat) o
type instance Prp i U        = U
type instance Prp i (O p)    = O (i ': p)
type instance Prp i (o1, o2) = (Prp i o1, Prp i o2)	-- Requires undec. inst


-- Note: "(o1,o2) >>> U" will end up as (U,U) instead of U. Does that matter?  

type family o1 >>> o2
type instance U        >>> o = U
type instance (O p)    >>> o = Prj p o
type instance (o1, o2) >>> o = (o1 >>> o, o2 >>> o)


-- Justification of
-- 
--    Prj '[] (o1,o2) = U
--
-- Yes, in fact observable if both o1 and o2 are. But we have no
-- way of representing that a single "given" position is observable
-- by drawing from *multiple* "outcome" positions. And it would be
-- be rather (somewhat?) complicated and probably unnecessary to extend it to
-- allow observability based on multiple positions. 
--
-- Justificatio of
--
--    Prj p (O p') = O p'
--
-- The intution is that if part of the "given" is observable from position
-- p of the "outcome" of the first CP, and *all* of the given of the
-- second CP is observable from position p' of the "outcome" of the
-- second CP, then surely the part under consideration of the given
-- of the composed CP is observable from postion p' of the "outcome"
-- of the composed CP.
--
-- Formally:
-- ow p = f o pi_p
-- ow p' = g o pi_p'
--
-- ow (p >>> p') = (f o pi_p o g) o pi_p'
--
-- So clearly, an observation witness of the form h o pi_p' exists
-- for the composed CP.


type family Prj (p :: [Nat]) o
type instance Prj p        U        = U
type instance Prj p        (O p')   = O p'
type instance Prj '[]      (o1, o2) = U
type instance Prj (1 ': p) (o1, o2) = Prj p o1
type instance Prj (2 ': p) (o1, o2) = Prj p o2


-- select (U, O [1]) (O [2]) (1,2) (3,4) = (3, unobservable)
--
-- Thus we want: (U, O [1]) &&& (O [2]) = (O [2,2], U)
--
-- select ((U, O[2]), O [1]) (O [2]) (1,2) (3,4) = ((4, unobservable), unobservable)
--
-- Thus we want: ((U,O), O [1]) &&& (O [2]) = ((O [2,2], U), U)

type family o1 &&& o2
type instance U          &&& U          = U
type instance (O p1)     &&& U          = Prp 1 (O p1)
type instance U          &&& (O p2)     = Prp 2 (O p2)
type instance (O p1)     &&& (O p2)     = U
type instance (o11, o12) &&& U          = Prp 1 (o11, o12)
type instance (o11, o12) &&& (O p2)     = (Sel (Prp 1 o11) (Prp 2 (O p2)),
                                           Sel (Prp 1 o12) (Prp 2 (O p2)))
type instance U          &&& (o21, o22) = Prp 2 (o21, o22)
type instance (O p1)     &&& (o21, o22) = (Sel (Prp 1 (O p1)) (Prp 2 o21),
                                           Sel (Prp 1 (O p1)) (Prp 2 o22))
type instance (o11, o12) &&& (o21, o22) = (Sel (Prp 1 o11) (Prp 2 o21),
                                           Sel (Prp 1 o12) (Prp 2 o22))


type family Sel o1 o2
type instance Sel U          U          = U
type instance Sel (O p1)     U          = (O p1)
type instance Sel U          (O p2)     = (O p2)
type instance Sel (O p1)     (O p2)     = U
type instance Sel (o11, o12) U          = (o11, o12)
type instance Sel (o11, o12) (O p2)     = (Sel o11 (O p2), Sel o12 (O p2))
type instance Sel U          (o21, o22) = (o21, o22)
type instance Sel (O p1)     (o21, o22) = (Sel (O p1) o21, Sel (O p1) o22)
type instance Sel (o11, o12) (o21, o22) = (Sel o11 o21, Sel o12 o22)


class Fusable o a where
    fuse :: o -> a -> a -> a


instance Fusable U a where
    fuse o x_e x_o = x_e


instance Fusable (O p) a where
    fuse o x_e x_o = x_o


instance (Fusable o1 a, Fusable o2 b) => Fusable (o1, o2) (a, b) where
    fuse o (x1_e, x2_e) (x1_o, x2_o) =
        (fuse (fst o) x1_e x1_o, fuse (snd o) x2_e x2_o)


class Selectable o1 o2 a where
    select :: o1 -> o2 -> a -> a -> a


instance Selectable U U a where
    select _ _ _ _ = unobservable


instance Selectable (O p1) U a where
    select _ _ x1 _ = x1


instance Selectable U (O p2) a where
    select _ _ _ x2 = x2


-- Note: above, an observation that draws from one position AND another,
-- is considered unobservable.
--
-- Here, we're statically ruling out an observation that can be made
-- from one position OR another.
--
-- We could, of course, simply consider it unobservable. This is what
-- we do for "many" anyway! 
--
-- Consider (identity &&& (identity &&& identity)).
-- Suppose O + O = U. Then O + (O + O) = O + U = O
--
-- So, actually, it's xor! But xor is algebraically well-behaved.
-- So why not? Would be good not to rule out networks unnecessarily.
-- 
-- We need to insist that all data is observable anyway, so bad
-- networks will be taken care of.

instance Selectable (O p1) (O p2) a where
    select _ _ _ _ = unobservable


-- I don't think we need other instances for "atomic" types a:
-- As long as observability products are only introduced by
-- combinators like *** and &&&, then the type to go with it will
-- also be a tuple. We might run into trouble if we introduce

-- Well, consider (A *** B) &&& I. Surely we want this to be
-- observable if (A *** B) is unobservable.
--
-- Yes, but then the type a must be a pair! 

-- Justification for instances like: (o1, o2) (O p) (a, b)
-- (O p) says that the *entire* pair is observable along the
-- right path at a position p of its "outcome". This means that
-- the *components* of this pair are observable individually from
-- that very same position.
-- Formally:
--
-- (x,y) = f o pi_p
--
-- Thus:
--
-- x = fst o f o pi_p
--
-- so an obesrvation witness for x exists and is of the form g o pi_p. 


instance (Selectable o11 U a, Selectable o12 U b) =>
         Selectable (o11, o12) U (a, b) where
    select o1 o2 (x11, x12) (x21, x22) =
         (select (fst o1) o2 x11 x21, select (snd o1) o2 x12 x22)


instance (Selectable o11 (O p2) a, Selectable o12 (O p2) b) =>
         Selectable (o11, o12) (O p2) (a, b) where
    select o1 o2 (x11, x12) (x21, x22) =
         (select (fst o1) o2 x11 x21, select (snd o1) o2 x12 x22)


instance (Selectable U o21 a, Selectable U o22 b) =>
         Selectable U (o21, o22) (a, b) where
    select o1 o2 (x11, x12) (x21, x22) =
         (select o1 (fst o2) x11 x21, select o1 (snd o2) x12 x22)


instance (Selectable (O p1) o21 a, Selectable (O p1) o22 b) =>
         Selectable (O p1) (o21, o22) (a, b) where
    select o1 o2 (x11, x12) (x21, x22) =
         (select o1 (fst o2) x11 x21, select o1 (snd o2) x12 x22)


instance (Selectable o11 o21 a, Selectable o12 o22 b) =>
         Selectable (o11, o12) (o21, o22) (a, b) where
    select o1 o2 (x11, x12) (x21, x22) =
        (select (fst o1) (fst o2) x11 x21, select (snd o1) (snd o2) x12 x22)


------------------------------------------------------------------------------
-- Conditional Probability Arrow (CP)
------------------------------------------------------------------------------

-- Key notions and ideas:
--  * The input side is the "given", the output side the "outcome".
--    We use variable names based on "x", "y", "z" for givens and
--    outcomes such that names for the givens precedes names for
--    outcomes alphabetically, unless there are more suitable, specific,
--    names.
--  * There are two types of random variables:
--    - Parameters: estimated if not observed; must be of type double
--    - Data: must be observed; can be of any type (that can be observed).
--  * "Estimates" are fed forward. (Estimates for data are undefined. Data
--     must ultimately be observed.) We use suffix "_e" for estimates. 
--  * "Observations" are fed backwards. We use suffix "_o" for observations.
--  * "Fused" estimates and observations are fed forward to the computation
--    of the logpdf summands.
--  * Wherever a random variable is (locally) unobservable (specifically,
--    sequential composition), its "estimate" is used in place of its
--    observed value in the fused variable vector to be used for computing
--    the (local) logpdf summand.
--  * Observations (possibly of complementary variables
--    in the vector) must NOT come from different branches in a fan-out.
--    The reason is that we only know about *observability* (of the given
--    from the outcomes) locally. So if there is a choice between two
--    observations, even if we know they must be consistent, then picking
--    one over the other locally always risk being the wrong choice in
--    in a larger context. 
--  * The log pdf is applied to the *fused* given and outcomes.


-- Arguments to the function representing a CP:
--  * Estimate of the given
--  * Fused estimate and observation of given (for computation of logpdf
--    summand).
--  * Observation of the outcome
--
-- Result of the function representing a CP:
--  * Estimate of the outcome
--  * Observation of the given
--  * Summand of overall log pdf(parameters | data)
--  * Parameters (estimated or observed)
--  * Continuation

data CP o a b = CP {
    cp         :: a -> Prob b,
    initEstim  :: a -> a -> b -> Prob (b, a, Double, Parameters, E o a b)
}


instance HasObs (CP o a b) o where
    obs _ = undefined

{-
instance HasObs (CP o a b) o where
    type Obs (CP o a b) = o
    obs _ = undefined
-}


-- Computation of one estimation step, given wheteher or no to keep
-- the previous step.

data E o a b = E {
    estimate :: Bool -> a -> a -> b -> Prob (b, a, Double, Parameters, E o a b)
}


instance HasObs (E o a b) o where
    obs _ = undefined


-- This assumes that the observability can be inferred.
mkCP :: (a -> a -> b -> Prob (b, a, Double, Parameters, E o a b)) -> CP o a b
mkCP e = CP { initEstim = e }

-- A parameter
--
-- Ultimately we want "parameter" to to be a transformer of a generator.
-- Something like: parameter "a" $ uniform
--
-- One could also consider marking data, of course. "data $ bernoulli". 
-- Any point?
--
-- Also, we should really look into a version that in turn is parametrised.

uniformParam :: Name -> Double -> Double -> CP U () Double
uniformParam pn a b = mkCP $ \_ _ y_o -> do
    (y_e, v) <- fmap meanVar $ replicateM 10 (uniform a b)
    let sd   = (sqrt v) / 2	-- A guess as good as another ...
	x_o  = unobservable    
        lpds = lpdfUniform a b y_o
    return (y_e, x_o, lpds, M.singleton pn (PVScalar y_o), upE sd y_e y_e)
    where
        upE sd y_e_pp y_e_p = E $ \k _ _ y_o -> do	-- "k" for "keep"
            let y_e' = if k then y_e_p else y_e_pp
            y_e <- gaussian y_e' sd
            let x_o  = unobservable    
                lpds = lpdfUniform a b y_o
            return (y_e, x_o, lpds, M.singleton pn (PVScalar y_e),
                    upE sd y_e' y_e)

-- A data generator

bernoulliData :: CP U Double Bool
bernoulliData = mkCP $ \p_e p_f b_o -> do
    let b_e  = inestimable
        p_o  = unobservable
        lpds = lpdfBernoulli p_f b_o
    return (b_e, p_o, lpds, M.empty, bdE)
    where
        bdE = E $ \_ p_e p_f b_o -> do
            let b_e  = inestimable
                p_o  = unobservable
                lpds = lpdfBernoulli p_f b_o
            return (b_e, p_o, lpds, M.empty, bdE)

                                                  
-- Identity
-- Do the inputs and outputs assume a different meaning for
-- plain routing functions? What's clear is that e.g. seq. composition
-- by identity must not change anything.

identity :: CP (O '[]) a a
identity = rarr id id


constant :: b -> CP U a b
constant y = arr (const y)


-- Arr

arr :: (a -> b) -> CP U a b
arr f = mkCP $ \x_e _ _ -> do
    let y_e  = f x_e
        x_o  = unobservable
        lpds = neutrSummand
    return (y_e, x_o, lpds, M.empty, arrE)
    where
        arrE = E $ \_ x_e _ _ -> do
            let y_e  = f x_e
                x_o  = unobservable
                lpds = neutrSummand
            return (y_e, x_o, lpds, M.empty, arrE)

instance A.ArrowArr (CP U) where
  arr = arr

-- Rarr: reversible arrow
-- 
-- TODO: Need to verify that the reverse mapping does not affect
-- the lpd. But as long as fixed factor/constant term, does not
-- make it harder to find the optimum. There were some calculations.

rarr :: (a -> b) -> (b -> a) -> CP (O '[]) a b
rarr f fi = mkCP $ \x_e _ y_o -> do
    let y_e  = f x_e
        x_o  = fi y_o
        lpds = neutrSummand
    return (y_e, x_o, lpds, M.empty, rarrE)
    where
        rarrE = E $ \_ x_e _ y_o -> do
            let y_e  = f x_e
                x_o  = fi y_o
                lpds = neutrSummand
            return (y_e, x_o, lpds, M.empty, rarrE)


-- Parallel composition

(***) :: CP o1 a b -> CP o2 c d -> CP (o1 *** o2) (a,c) (b,d)
cp1 *** cp2 = mkCP $ \(x1_e, x2_e) ~(x1_f, x2_f) ~(y1_o, y2_o) -> do
    (y1_e, x1_o, lpds1, ps1, e1) <- initEstim cp1 x1_e x1_f y1_o 
    (y2_e, x2_o, lpds2, ps2, e2) <- initEstim cp2 x2_e x2_f y2_o 
    return ((y1_e, y2_e), (x1_o, x2_o), lpds1 + lpds2, M.union ps1 ps2,
            e1 *** e2)
    where
        e1 *** e2 = E $ \k (x1_e, x2_e) ~(x1_f, x2_f) ~(y1_o, y2_o) -> do
            (y1_e, x1_o, lpds1, ps1, e1') <- estimate e1 k x1_e x1_f y1_o 
            (y2_e, x2_o, lpds2, ps2, e2') <- estimate e2 k x2_e x2_f y2_o 
            return ((y1_e, y2_e), (x1_o, x2_o), lpds1 + lpds2, M.union ps1 ps2,
                    e1' *** e2')

instance (o3 ~ (o1 *** o2)) => A.ArrowParallel (CP o1) (CP o2) (CP o3) where
  (***) = (***)

-- Sequential composition
-- Data flow:
-- y_e <- x_e & generators
-- z_e <- y_e & generators
-- y_o <- z_o
-- x_o <- y_o

(>>>) :: Fusable o2 b => CP o1 a b -> CP o2 b c -> CP (o1 >>> o2) a c
(>>>) cp1 cp2 = mkCP $ \x_e x_f z_o -> do
    fp <- mfix $ \ ~(_, y_f') -> do
        (y_e, x_o, lpds1, ps1, e1) <- initEstim cp1 x_e x_f y_f'
        (z_e, y_o, lpds2, ps2, e2) <- initEstim cp2 y_e y_f' z_o
        let y_f = fuse (obs cp2) y_e y_o
        return ((z_e, x_o, lpds1 + lpds2, M.union ps1 ps2, e1 >>> e2), y_f)
    return (fst fp)
    where
        e1 >>> e2 = E $ \k x_e x_f z_o -> do
            fp <- mfix $ \ ~(_, y_f') -> do
                (y_e, x_o, lpds1, ps1, e1') <- estimate e1 k x_e x_f y_f'
                (z_e, y_o, lpds2, ps2, e2') <- estimate e2 k y_e y_f' z_o
                let y_f = fuse (obs e2) y_e y_o
                return ((z_e, x_o, lpds1 + lpds2, M.union ps1 ps2,
			 e1' >>> e2'),
                        y_f)
            return (fst fp)

instance (o3 ~ (o1 >>> o2)) => A.ArrowSequence (CP o1) (CP o2) (CP o3) where
  type ArrowSequenceCts (CP o1) (CP o2) (CP o3) a b c = (Fusable o2 b)
  (>>>) = (>>>)

-- Fan out
(&&&) :: Selectable o1 o2 a => CP o1 a b -> CP o2 a c -> CP (o1 &&& o2) a (b,c)
(&&&) cp1 cp2 = mkCP $ \x_e x_f ~(y_o, z_o) -> do
    (y_e, x1_o, lpds1, ps1, e1) <- initEstim cp1 x_e x_f y_o
    (z_e, x2_o, lpds2, ps2, e2) <- initEstim cp2 x_e x_f z_o
    let x_o = select (obs cp1) (obs cp2) x1_o x2_o
    return ((y_e, z_e), x_o, lpds1 + lpds2, M.union ps1 ps2, e1 &&& e2) 
    where
        e1 &&& e2 = E $ \k x_e x_f ~(y_o, z_o) -> do
            (y_e, x1_o, lpds1, ps1, e1') <- estimate e1 k x_e x_f y_o
            (z_e, x2_o, lpds2, ps2, e2') <- estimate e2 k x_e x_f z_o
            let x_o = select (obs e1) (obs e2) x1_o x2_o
            return ((y_e, z_e), x_o, lpds1 + lpds2, M.union ps1 ps2,
                    e1' &&& e2')

instance (o3 ~ (o1 &&& o2)) => A.ArrowFanOut (CP o1) (CP o2) (CP o3) where
  type ArrowFanOutCts (CP o1) (CP o2) (CP o3) a b c = (Selectable o1 o2 a)
  (&&&) = (&&&)

{-
data CP o a b = CP {
    cp         :: a -> Prob b,
    initEstim  :: a -> a -> b -> Prob (b, a, Double, Parameters, E o a b)
}

data E o a b = E {
    estimate :: Bool -> a -> a -> b -> Prob (b, a, Double, Parameters, E o a b)
}
-}

first :: forall o a b c. CP o a b -> CP o (a, c) (b, c)
first (CP cp initEstim) = CP 
  { cp = \(a, c) -> fmap (\b -> (b, c)) (cp a)
  , initEstim = \(a, c0) (a', c1) (b, c2) -> 
      let g :: E o a b -> E o (a,c) (b,c)
          g (E est) = E $ \bool (a, c) (a', c') (b, c'') -> fmap f (est bool a a' b)
          f :: (b, a, Double, Parameters, E o a b) 
            -> ((b, c), (a, c), Double, Parameters, E o (a,c) (b,c))
          f (b', a'', d, p, e) = ((b', c2), (a'', c0 {- or c1? -}), d, p, g e)
      in fmap f (initEstim a a' b)
  }

second :: CP o a b -> CP o (c, a) (c, b)
second (CP cp initEstim) = CP 
  { cp = undefined
  , initEstim = undefined
  }

instance A.ArrowSelect (CP o) (CP o) where
  first = first
  second = second
  {-
  type ArrowSelectFstCts f g a b c :: Constraint
  type ArrowSelectFstCts f g a b c = ()
  type ArrowSelectSndCts f g a b c :: Constraint
  type ArrowSelectSndCts f g a b c = ()
  first  :: (ArrowSelectFstCts f g a b c) => f a b -> g (a, c) (b, c)
  second :: (ArrowSelectSndCts f g a b c) => f a b -> g (c, a) (c, b)  
  -}

-- N-ary fan out. The integer parameter is only used in the forward
-- direction; ignored when estimating.
--
-- Interesting: we need to maintain independent instances ...
-- so the observation had better not change! In fact, 
-- the observation *should* never change, so should really be
-- part of the local state ... but not easy to tell obsevations
-- apart from estimates and the type structure would change ...
--
-- We don't know number of observations initially ... so pick
-- pick infinitey many ... 
--
-- This would also be solved if we only observe once: the initialization
-- happens when we do the first observation, and at that point we do
-- know how many instances to create, can apply each to its particular
-- observation once and for all.
-- 
-- Might be that the y_o argument should come first to facilitate
-- partial application ... I guess all state arguments need to come
-- first anyway.

{- 
-- Old code
many :: Int -> CP a b -> CP a [b]
many n cp = CP $ \x_e x_f ys_o -> do
    (ys_e, _, lpdss) <- fmap unzip3 (mapM (unCP cp x_e x_f) ys_o)
    return (ys_e, unobservable, sum lpdss)
-}

-- "repeat cp" as we don't know the number of observations statically
many :: Int -> CP o a b -> CP U a [b]
many n cp = mkCP $ \x_e x_f ys_o -> do
    (ys_e, _, lpdss, pss, es) <-
        fmap unzip5
             (sequence [ initEstim cp x_e x_f y_o
                       | (cp, y_o) <- zip (repeat cp) ys_o ])
    return (ys_e, unobservable, sum lpdss, unionParams pss, manyE es)
    where
        manyE es = E $ \k x_e x_f ys_o -> do
            (ys_e, _, lpdss, pss, es') <-
                fmap unzip5
                     (sequence [ estimate e k x_e x_f y_o
                               | (e, y_o) <- zip es ys_o ])
            return (ys_e, unobservable, sum lpdss, unionParams pss, manyE es')

        unionParams pss = 
            M.unions [ M.mapKeys (\pn -> pn ++ "_" ++ show n) ps
                     | (ps, n) <- zip pss [1..] ]

{-

runCP :: RandomGen g => g -> CP a b -> a -> b -> (b, a, Double)
runCP g cp a b = runProb g (unCP cp a a b)

-}

-- Metropolis-Hastings Chain
data MHC = forall o a b . MHC {
    keep_p    :: Bool,
    lpd_p     :: Double,
    ps_p      :: Parameters,
    given     :: a,
    outcome   :: b,
    estimator :: E o a b 
}

mhcNext :: MHC -> Prob (Double, Parameters, MHC)
mhcNext (MHC {keep_p, lpd_p, ps_p, given = x, outcome = y, estimator = e}) = do
    (_, _, lpd, ps, e') <- estimate e keep_p x x y
    if lpd > lpd_p then
        return (lpd, ps,
                MHC {
                    keep_p    = True,
                    lpd_p     = lpd,
                    ps_p      = ps,
                    given     = x,
                    outcome   = y,
                    estimator = e'
                })
    else do
        u <- unit
        if u < exp (lpd - lpd_p) then
            return (lpd, ps,
                    MHC {
                        keep_p    = True,
                        lpd_p     = lpd,
                        ps_p      = ps,
                        given     = x,
                        outcome   = y,
                        estimator = e'
                    })
        else
            return (lpd_p, ps_p,
                    MHC {
                        keep_p    = False,
                        lpd_p     = lpd_p,
                        ps_p      = ps_p,
                        given     = x,
                        outcome   = y,
                        estimator = e'
                    })


mhcNth :: Int -> MHC -> Prob (Double, Parameters, MHC)
mhcNth n mhc
    | n < 0     = error "Negative index into MH Chain"
    | n == 0    = mhcNext mhc
    | otherwise = do
        (_, _, mhc') <- mhcNext mhc
        mhcNth (n-1) mhc'


-- Metropolis-Hastings estimation
--
-- Parameters:
-- b ..........	Length of "burn in": the initial part of the chain to discard.
-- t ..........	Number of consecutive chain elements to discard to generate
--		independent samples (10 seems to suffice for our simple ex.)
-- n ..........	Number of samples to generate.
-- cp .........	Model
-- x ..........	Given
-- y .......... Outcome (observed data)
--
-- Returns:
-- List of pairs of log of pdf and corresponding parameter estimate.

estimateMH :: Int -> Int -> Int -> CP o a b -> a -> b
              -> Prob [(Double, Parameters)]
estimateMH b t n cp x y = do
    (_, _, lpd, ps, e0) <- initEstim cp x x y
    let mhc = MHC {
	          keep_p    = True,
                  lpd_p     = lpd,
                  ps_p      = ps,
		  given     = x,
		  outcome   = y,
		  estimator = e0
              }
    (_, _, mhc') <- mhcNth (max b 0) mhc
    emhAux [] (max n 0) mhc'
    where
        t' = max t 0

	-- Order shouldn't matter, but potentially useful for debugging
	-- to ensure returned in order generated.
        emhAux rlpdpss 0 _   = return (reverse rlpdpss)
        emhAux rlpdpss n mhc = do
            (lpd, ps, mhc') <- mhcNth t' mhc
            emhAux ((lpd, ps) : rlpdpss) (n - 1) mhc'

{-
runCP :: RandomGen g => g -> Int -> CP o a b -> a -> b -> [Double]
runCP _ 0 _  _ _ = []
runCP g n cp a b = runProb g $ do
    (_, _, lpd, e) <- initEstim cp a a b
    rpAux (n - 1) [lpd] e 
    where
        rpAux 0 rlpds _  = return (reverse rlpds) 
        rpAux n rlpds e = do
	    (_, _, lpd, e') <- estimate e True a a b
            rpAux (n - 1) (lpd : rlpds) e' 
-}

runCP :: RandomGen g =>
             g -> Int -> Int -> Int -> CP o a b -> a -> b
             -> [(Double, Parameters)]
runCP g b t n cp x y = runProb g $ estimateMH b t n cp x y


plotScalarParaDist :: Bool -> Name -> [Parameters] -> Int -> IO ()
plotScalarParaDist toFile pn pss b
    | b < 1     = error "Invalid number of bins"
    | toFile    = gnuplotToPS (pn ++ ".eps") $ Histo b (map (lookup pn) pss)
    | otherwise = gnuplotOnScreen $ Histo b (map (lookup pn) pss)
    where
        lookup pn ps =
            case M.lookup pn ps of
                Nothing -> error $ "No parameter \"" ++ pn ++ "\""
                Just p  ->
                    case p of
                        PVScalar d -> d
                        _          -> error $
                                          "Parameter \""
                                          ++ pn
                                          ++ "\" is not a scalar"

findScalarParams :: Name -> [(Double,Parameters)] -> (Double -> Bool) -> [(Double,Double)]
findScalarParams pn dpss p =
    [ (d, x) | (d, ps) <- dpss, let x = lookup pn ps, p x ]
    where
        lookup pn ps =
            case M.lookup pn ps of
                Nothing -> error $ "No parameter \"" ++ pn ++ "\""
                Just p  ->
                    case p of
                        PVScalar d -> d
                        _          -> error $
                                          "Parameter \""
                                          ++ pn
                                          ++ "\" is not a scalar"


unobservable = error "Unobservable"

inestimable = error "Data: inestimable, must be observed"

neutrSummand :: Double
neutrSummand = 0


------------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------------

-- Type sig. not needed. Good as suggest typing is roubust/supports inference.
-- coin :: CP U () Bool
coin = uniformParam "p" 0 1 >>> bernoulliData


-- coinI :: CP U () Bool
coinI = uniformParam "p" 0 1 >>> (identity >>> bernoulliData)


-- coinI' :: CP U () Bool
coinI' = (uniformParam "p" 0 1 >>> identity) >>> bernoulliData


-- Coin where parameter can be observed
-- coinOP :: CP U () (Double, Bool)
coinOP = uniformParam "p" 0 1 >>> (identity &&& bernoulliData)


-- twoCoins :: CP U () (Bool, Bool)
twoCoins = uniformParam "p" 0 1
           >>> ((identity &&& bernoulliData)
                >>> (bernoulliData *** identity))

twoCoins' = coinOP >>> (bernoulliData *** identity)


-- Two coins where the parameter can be observed

-- twoCoinsOP :: CP U () (Bool, (Bool, Double))
twoCoinsOP = uniformParam "p" 0 1
             >>> ((bernoulliData &&& identity)
                  >>> (identity *** (bernoulliData &&& identity)))


fourCoins :: CP U () (Bool, (Bool, (Bool, Bool)))
fourCoins = twoCoinsOP
            >>> (identity *** (identity *** (bernoulliData &&& bernoulliData)))


manyCoins :: CP U () [Bool]
manyCoins = uniformParam "p" 0 1 >>> many 10 bernoulliData


-- Two coins with unknown bias, but where (we think) we know that the
-- likelihood of tails (True) of one is half of the likelihood for
-- tails of the other.

strangeCoins :: CP U Double (Bool, Bool)
strangeCoins = (identity &&& arr (/2))
               >>> (bernoulliData *** bernoulliData)

strangeCoinsMany :: CP U () [(Bool, Bool)]
strangeCoinsMany = uniformParam "p" 0 1 >>> many 10 strangeCoins


strangeCoins_0p5 :: CP U () (Bool, Bool)
strangeCoins_0p5 = constant 0.5 >>> strangeCoins


------------------------------------------------------------------------------
-- Light house example
------------------------------------------------------------------------------

-- alpha: distance of light-house along shore
-- beta: distance of light-house from shore

cauchy :: (Double, Double) -> Prob Double
cauchy (alpha, beta) = do
    theta <- uniform (-pi/2) (pi/2)
    return (alpha + beta * tan theta)

lpdfCauchy :: (Double, Double) -> Double -> Double
lpdfCauchy (alpha, beta) x = log beta - log pi - log (beta^2 + (x-alpha)^2)


lightHouseTruth :: Int -> Double -> Double -> Prob [Double]
lightHouseTruth n alpha beta = replicateM n (cauchy (alpha, beta))


-- Genereate a set of samples e.g. like this:
-- xs <- newStdGen >>= \g -> return (runProb g (truth 1000 1.0 2.0))

xs_200_1_2 :: IO [Double]
xs_200_1_2 = do
    g <- newStdGen
    return (runProb g (lightHouseTruth 200 1.0 2.0))


xs_500_1_2 :: IO [Double]
xs_500_1_2 = do
    g <- newStdGen
    return (runProb g (lightHouseTruth 500 1.0 2.0))


xs_1000_1_2 :: IO [Double]
xs_1000_1_2 = do
    g <- newStdGen
    return (runProb g (lightHouseTruth 1000 1.0 2.0))

xs_200_8_2 :: IO [Double]
xs_200_8_2 = do
    g <- newStdGen
    return (runProb g (lightHouseTruth 200 8.0 2.0))


lightHouseFlashes :: CP U (Double, Double) Double
lightHouseFlashes = mkCP $ \p_e p_f b_o -> do
    let b_e  = inestimable
        p_o  = unobservable
        lpds = lpdfCauchy p_f b_o
    return (b_e, p_o, lpds, M.empty, bdE)
    where
        bdE = E $ \_ p_e p_f b_o -> do
            let b_e  = inestimable
                p_o  = unobservable
                lpds = lpdfCauchy p_f b_o
            return (b_e, p_o, lpds, M.empty, bdE)

lightHouse :: CP U () [Double]
lightHouse = (uniformParam "alpha" (-50) 50
              &&& uniformParam "beta" 0 20)
             >>> many 10 lightHouseFlashes


lightHouse2 :: CP U () [Double]
lightHouse2 = (uniformParam "alpha" (-50) 50
               &&& (uniformParam "beta" 0 20
                    &&& many 10 (uniformParam "theta" (-pi/2) (pi/2))))
              >>> arr id {- identity -} *** arr (\ ~(beta, thetas) ->
                                           map (\theta -> beta * tan theta)
                                               thetas)
              >>> arr (\ ~(alpha, xs) -> map (+alpha) xs)

test :: CP U () Double
test = uniformParam "a" 0 1
       &&& (uniformParam "b" 0 2
            &&& uniformParam "c" 0 5)
       >>> identity *** arr (uncurry (+))
       >>> arr (uncurry (+))


main = do
    xs <- xs_200_8_2
    g <- newStdGen
    let lpdpss = runCP g 1000 10 100000 lightHouse () xs
    plotScalarParaDist True "alpha" (map snd lpdpss) 50
    plotScalarParaDist True "beta" (map snd lpdpss) 50
