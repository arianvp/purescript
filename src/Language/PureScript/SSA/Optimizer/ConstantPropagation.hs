{-# LANGUAGE TupleSections #-}
module Language.PureScript.SSA.Optimizer.ConstantPropagation
  ( propagateConstants
  ) where

import Control.Monad.State (evalState, gets, modify, State)
import Data.Map (Map)
import Data.Map as Map
import Language.PureScript.SSA.Graph

type Env = Map InstID Value

type M = State Env

propagateConstants :: [(BlockID, Block)] -> [(BlockID, Block)]
propagateConstants bs = evalState (propagateConstants' bs) Map.empty

propagateConstants' :: [(BlockID, Block)] -> M [(BlockID, Block)]
propagateConstants' = mapM $ \(bID, b) -> (bID,) <$> propagateConstants'' b

propagateConstants'' :: Block -> M Block
propagateConstants'' = mapM propagateConstants'''

propagateConstants''' :: (InstID, Inst) -> M (InstID, Inst)
propagateConstants''' (iID, i) = (iID,) <$>
  case i of
    AliasInst v -> do
      v' <- inject v
      modify $ Map.insert iID v'
      return $ AliasInst v'

    ArrayInst vs -> ArrayInst <$> mapM inject vs

    ObjectInst ps -> ObjectInst <$> mapM (\(p, v) -> (p,) <$> inject v) ps

    AccessInst p v -> AccessInst p <$> inject v

    ObjectUpdateInst v ps ->
      ObjectUpdateInst
        <$> inject v
        <*> mapM (\(p, pv) -> (p,) <$> inject pv) ps

    CallInst f as -> CallInst <$> inject f <*> mapM inject as

    IntArithInst arith v w -> do
      v' <- inject v
      w' <- inject w
      case (v', w') of
        (IntValue a, IntValue b) -> do
          let r = IntValue (a `arith` b)
          modify $ Map.insert iID r
          return $ AliasInst r
        _ -> return $ IntArithInst arith v' w'

    GotoInst bID -> return $ GotoInst bID

    IfInst c tID eID -> do
      c' <- inject c
      case c' of
        BooleanValue True  -> return $ GotoInst tID
        BooleanValue False -> return $ GotoInst eID
        _ -> return $ IfInst c' tID eID

inject :: Value -> M Value
inject v@(InstValue iID) = do
  vm <- gets $ Map.lookup iID
  case vm of
    Just v' -> return v'
    Nothing -> return v
inject v = return v
