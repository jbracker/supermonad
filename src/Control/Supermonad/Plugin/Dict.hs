
-- | Provides the type for supermonad dictionaries.
module Control.Supermonad.Plugin.Dict 
  ( SupermonadDict
  , BindInst, ApplicativeInst, ReturnInst
  , emptyDict, insertDict, lookupDict
  , lookupDictBind, lookupDictApplicative, lookupDictReturn
  , allTyConsDict ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Monad ( join )

import InstEnv ( ClsInst(..) )
import TyCon ( TyCon )

-- | A dictionary associating supermonad type constructors with their
--   'Control.Supermonad.Bind', 'Control.Supermonad.Applicative' 
--   and 'Control.Supermonad.Return' instances.
--   This is essentially a lookup table for instances associated with a 
--   given type constructor.
--   Note that the bind instance is optional because a given 
--   type constructor may only be a superapplicative, but not a supermonad.
newtype SupermonadDict = SDict (M.Map TyCon (Maybe BindInst, ApplicativeInst, ReturnInst))

instance Monoid SupermonadDict where
  mappend (SDict a) (SDict b) = SDict $ mappend a b
  mempty = emptyDict

-- | Class instances of 'Control.Supermonad.Bind'.
type BindInst        = ClsInst

-- | Class instance of 'Control.Supermonad.Applicative'.
type ApplicativeInst = ClsInst

-- | Class instance of 'Control.Supermonad.Return'.
type ReturnInst      = ClsInst

-- | The empty supermonad dictionary.
emptyDict :: SupermonadDict
emptyDict = SDict $ M.empty

-- | Insert an entry into a supermonad dictionary.
insertDict :: TyCon -> Maybe BindInst -> ApplicativeInst -> ReturnInst 
           -> SupermonadDict -> SupermonadDict
insertDict tc mClsBind clsApp clsRet (SDict smDict) 
  = SDict $ M.insert tc (mClsBind, clsApp, clsRet) smDict

-- | Try to lookup an entry in a supermonad dictionary.
lookupDict :: TyCon -> SupermonadDict -> Maybe (Maybe BindInst, ApplicativeInst, ReturnInst)
lookupDict tc (SDict smDict) = M.lookup tc smDict

-- | Try to lookup the 'Control.Supermonad.Bind' instance of the type constructor.
lookupDictBind :: TyCon -> SupermonadDict -> Maybe BindInst
lookupDictBind tc smDict = join $ fmap (\(b, _, _) -> b) $ lookupDict tc smDict

-- | Try to lookup the 'Control.Supermonad.Applicative' instance of the type constructor.
lookupDictApplicative :: TyCon -> SupermonadDict -> Maybe ApplicativeInst
lookupDictApplicative tc smDict = fmap (\(_, a, _) -> a) $ lookupDict tc smDict

-- | Try to lookup the 'Control.Supermonad.Return' instance of the type constructor.
lookupDictReturn :: TyCon -> SupermonadDict -> Maybe ReturnInst
lookupDictReturn tc smDict = fmap (\(_, _, r) -> r) $ lookupDict tc smDict

-- | Retrieve the 'S.Set' of all type constructors in that have an entry in
--   the supermonad dictionary.
allTyConsDict :: SupermonadDict -> S.Set TyCon
allTyConsDict (SDict smDict) = M.keysSet smDict