
-- | Constant names that are used throughout the plugin.
module Control.Supermonad.Plugin.Names
  ( -- * Module Names
    supermonadModuleName
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

-- -----------------------------------------------------------------------------
-- Constant Names (Magic Numbers...)
-- -----------------------------------------------------------------------------

-- | Name of the "Control.Supermonad" module.
supermonadModuleName :: String
supermonadModuleName = "Control.Supermonad"

-- | Name of the "Control.Supermonad.Constrained" module.
supermonadCtModuleName :: String
supermonadCtModuleName = "Control.Supermonad.Constrained"

-- | Name of the 'Control.Supermonad.Bind' type class.
bindClassName :: String
bindClassName = "Bind"

-- | Name of the 'Control.Supermonad.Bind' type class.
returnClassName :: String
returnClassName = "Return"

-- | Name of the 'Data.Functor.Functor' class.
functorClassName :: String
functorClassName = "Functor"

-- | Name of the 'Control.Supermonad.Applicative' type class.
applicativeClassName :: String
applicativeClassName = "Applicative"

-- | Name of the "Control.Supermonad.Prelude" module.
supermonadPreludeModuleName :: String
supermonadPreludeModuleName = "Control.Supermonad.Prelude"

-- | Name of the "Control.Supermonad.Constrained.Prelude" module.
supermonadCtPreludeModuleName :: String
supermonadCtPreludeModuleName = "Control.Supermonad.Constrained.Prelude"

-- | Name of the "Data.Functor" module.
functorModuleName :: String
functorModuleName = "Data.Functor"