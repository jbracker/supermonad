 
module Control.Supermonad.Plugin.Solving 
  ( solveConstraintGroup
  ) where

import Control.Monad ( forM )

import TcRnTypes ( Ct )
import TyCon ( TyCon )
import Type ( TyVar )
import TcPluginM ( TcPluginM )

import Control.Supermonad.Plugin.Environment ( SupermonadPluginM, printMsg )
import Control.Supermonad.Plugin.Constraint ( WantedCt )
import Control.Supermonad.Plugin.Separation ( componentTopTyCons, componentTopTcVars )
import Control.Supermonad.Plugin.Utils ( associations )

solveConstraintGroup :: [WantedCt] -> SupermonadPluginM [Ct]
solveConstraintGroup ctGroup = do
  printMsg $ "Solving Group..."
  printMsg $ "Size = " ++ show (length ctGroup) ++ "; "
          ++ "Vars = " ++ show (length tyConVars) ++ "; "
          ++ "Base Size = " ++ show (length tyConBase) ++ "; "
          ++ "Associations = " ++ show (length assocs) ++ ";"
  
  validAssocs <- forM assocs $ \assoc -> do
    undefined
  
  return []
  
  where
    tyConVars :: [TyVar]
    tyConVars = componentTopTcVars ctGroup
    
    tyConBase :: [TyCon]
    tyConBase = componentTopTyCons ctGroup
    
    assocs :: [[(TyVar, TyCon)]]
    assocs = associations $ fmap (\tv -> (tv, tyConBase)) tyConVars