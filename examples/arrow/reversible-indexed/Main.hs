
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE GADTs #-}

-- Use the polymonad plugin.
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Super.Arrow.Plugin #-}

module Main 
  ( Rev, Iso(..)
  , rarr, run
  , main
  ) where

import Control.Category
import Control.Super.Arrow.Prelude hiding ( id, (.) )
import Control.Super.Arrow

main :: IO ()
main = return ()

data Iso a b = Iso (a -> b) (b -> a)

instance Category Iso where
  id = Iso id id
  (Iso f f') . (Iso g g') = Iso (f . g) (g' . f')

isoProd :: Iso a b -> Iso c d -> Iso (a,c) (b,d)
isoProd (Iso f f') (Iso g g') = Iso (\(a,c) -> (f a, g c)) (\(b,d) -> (f' b, g' d))

data Rev (rev :: Bool) (a :: *) (b :: *) where
  Rev :: Iso a b -> Rev 'True a b
  NonRev :: Rev 'False a b

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'True  'True  = 'True
  Or 'True  'False = 'True
  Or 'False 'True  = 'True
  Or 'False 'False = 'False

type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'False 'False = 'False
  And 'False 'True  = 'False
  And 'True  'False = 'False
  And 'True  'True  = 'True

rarr :: (a -> b) -> (b -> a) -> Rev 'True a b
rarr f g = Rev (Iso f g)

run :: Rev 'True a b -> Iso a b
run (Rev f) = f

instance Category (Rev 'True) where
  id = Rev id
  (Rev f) . (Rev g) = Rev (f . g)

instance ArrowArr (Rev 'False) where
  arr _f = NonRev

instance (t ~ And r s) => ArrowSequence (Rev r) (Rev s) (Rev t)  where
  (Rev f) >>> (Rev g) = Rev (g . f)
  (Rev _) >>> NonRev  = NonRev
  NonRev  >>> (Rev _) = NonRev
  NonRev  >>> NonRev  = NonRev

instance (r ~ s) => ArrowSelect (Rev r) (Rev s) where
  first  (Rev f) = Rev $ isoProd f id 
  first  NonRev  = NonRev
  second (Rev f) = Rev $ isoProd id f
  second NonRev  = NonRev

instance (r ~ t, s ~ t) => ArrowParallel (Rev r) (Rev s) (Rev t) where
  (Rev f) *** (Rev g) = Rev $ isoProd f g
  NonRev  *** NonRev  = NonRev

instance (r ~ t, s ~ t) => ArrowFanOut (Rev r) (Rev s) (Rev t) where
  (Rev (Iso f f')) &&& (Rev (Iso g _g')) = Rev $ Iso (\a -> (f a, g a)) (\(b, _c) -> f' b)
  -- If we are talking about actual isomorphism ignoring 'c' should not be a problem.
  NonRev &&& NonRev = NonRev









