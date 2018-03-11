
-- | Constant names that are used throughout the plugin.
module Control.Super.Plugin.Names
  ( -- * Types
    PluginModuleName
  , PluginClassName
    -- * Supermonad Module Names
  , supermonadModuleName
  , supermonadCtModuleName
  , legacySupermonadModuleName
  , legacySupermonadCtModuleName
  , supermonadPreludeModuleName
  , supermonadCtPreludeModuleName
  , legacySupermonadPreludeModuleName
  , legacySupermonadCtPreludeModuleName
  , functorModuleName
    -- * Supermonad Class Names
  , bindClassName
  , returnClassName
  , functorClassName
  , applicativeClassName
    -- * Superarrow Module Names
  , superarrowModuleName
  , superarrowCtModuleName
    -- * Superarrow Class Names
  , arrowArrClassName
  , arrowSequenceClassName
  , arrowSelectClassName
  , arrowParallelClassName
  , arrowFanOutClassName
  ) where

-- | Type of module names the plugin uses.
type PluginModuleName = String

-- | Type of class names the plugin uses.
type PluginClassName = String  

-- -----------------------------------------------------------------------------
-- Constant Supermonad Names (Magic Numbers...)
-- -----------------------------------------------------------------------------

-- | Name of the "Control.Supermonad" module.
legacySupermonadModuleName :: PluginModuleName
legacySupermonadModuleName = "Control.Supermonad"

-- | Name of the "Control.Super.Monad" module.
supermonadModuleName :: PluginModuleName
supermonadModuleName = "Control.Super.Monad"

-- | Name of the "Control.Supermonad.Constrained" module.
legacySupermonadCtModuleName :: PluginModuleName
legacySupermonadCtModuleName = "Control.Supermonad.Constrained"

-- | Name of the "Control.Super.Monad.Constrained" module.
supermonadCtModuleName :: PluginModuleName
supermonadCtModuleName = "Control.Super.Monad.Constrained"

-- | Name of the @Bind@ type class.
--   Also used as dictionary key for the @Bind@ class.
bindClassName :: PluginClassName
bindClassName = "Bind"

-- | Name of the @Return@ type class.
--   Also used as dictionary key for the @Return@ class.
returnClassName :: PluginClassName
returnClassName = "Return"

-- | Name of the @Functor@ class.
--   Also used as dictionary key for the @Functor@ class.
functorClassName :: PluginClassName
functorClassName = "Functor"

-- | Name of the @Applicative@ type class.
--   Also used as dictionary key for the @Applicative@ class.
applicativeClassName :: PluginClassName
applicativeClassName = "Applicative"

-- | Name of the "Control.Supermonad.Prelude" module.
legacySupermonadPreludeModuleName :: PluginModuleName
legacySupermonadPreludeModuleName = "Control.Supermonad.Prelude"

-- | Name of the "Control.Super.Monad.Prelude" module.
supermonadPreludeModuleName :: PluginModuleName
supermonadPreludeModuleName = "Control.Super.Monad.Prelude"

-- | Name of the "Control.Supermonad.Constrained.Prelude" module.
legacySupermonadCtPreludeModuleName :: PluginModuleName
legacySupermonadCtPreludeModuleName = "Control.Supermonad.Constrained.Prelude"

-- | Name of the "Control.Super.Monad.Constrained.Prelude" module.
supermonadCtPreludeModuleName :: PluginModuleName
supermonadCtPreludeModuleName = "Control.Super.Monad.Constrained.Prelude"

-- | Name of the "Data.Functor" module.
functorModuleName :: PluginModuleName
functorModuleName = "Data.Functor"

-- -----------------------------------------------------------------------------
-- Constant Superarrow Names (Magic Numbers...)
-- -----------------------------------------------------------------------------

-- | Name of the "Control.Super.Arrow" module.
superarrowModuleName :: PluginModuleName
superarrowModuleName = "Control.Super.Arrow"

-- | Name of the "Control.Super.Arrow.Constrained" module.
superarrowCtModuleName :: PluginModuleName
superarrowCtModuleName = "Control.Super.Arrow.Constrained"

-- | Name of the @ArrowArr@ type class.
--   Also used as dictionary key for the @ArrowArr@ class.
arrowArrClassName :: PluginClassName
arrowArrClassName = "ArrowArr"

-- | Name of the @ArrowSequence@ class.
--   Also used as dictionary key for the @ArrowSequence@ class.
arrowSequenceClassName :: PluginClassName
arrowSequenceClassName = "ArrowSequence"

-- | Name of the @ArrowSelect@ type class.
--   Also used as dictionary key for the @ArrowSelect@ class.
arrowSelectClassName :: PluginClassName
arrowSelectClassName = "ArrowSelect"

-- | Name of the @ArrowParallel@ class.
--   Also used as dictionary key for the @ArrowParallel@ class.
arrowParallelClassName :: PluginClassName
arrowParallelClassName = "ArrowParallel"

-- | Name of the @ArrowFanOut@ class.
--   Also used as dictionary key for the @ArrowFanOut@ class.
arrowFanOutClassName :: PluginClassName
arrowFanOutClassName = "ArrowFanOut"




