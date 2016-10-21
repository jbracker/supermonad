
-- | Constant names that are used throughout the plugin.
module Control.Supermonad.Plugin.Names
  ( -- * Types
    PluginModuleName
  , PluginClassName
    -- * Module Names
  , supermonadModuleName
  , supermonadCtModuleName
  , supermonadPreludeModuleName
  , supermonadCtPreludeModuleName
  , functorModuleName
    -- * Class Names
  , bindClassName
  , returnClassName
  , functorClassName
  , applicativeClassName
  ) where

-- | Type of module names the plugin uses.
type PluginModuleName = String

-- | Type of class names the plugin uses.
type PluginClassName = String  

-- -----------------------------------------------------------------------------
-- Constant Names (Magic Numbers...)
-- -----------------------------------------------------------------------------

-- | Name of the "Control.Supermonad" module.
supermonadModuleName :: PluginModuleName
supermonadModuleName = "Control.Supermonad"

-- | Name of the "Control.Supermonad.Constrained" module.
supermonadCtModuleName :: PluginModuleName
supermonadCtModuleName = "Control.Supermonad.Constrained"

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
supermonadPreludeModuleName :: PluginModuleName
supermonadPreludeModuleName = "Control.Supermonad.Prelude"

-- | Name of the "Control.Supermonad.Constrained.Prelude" module.
supermonadCtPreludeModuleName :: PluginModuleName
supermonadCtPreludeModuleName = "Control.Supermonad.Constrained.Prelude"

-- | Name of the "Data.Functor" module.
functorModuleName :: PluginModuleName
functorModuleName = "Data.Functor"