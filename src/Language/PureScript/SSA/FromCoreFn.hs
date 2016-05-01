{-# LANGUAGE TupleSections #-}
module Language.PureScript.SSA.FromCoreFn
  (
  ) where

import Control.Monad.State (evalState, gets, modify, State)
import Data.Map (Map)
import Data.Map as Map
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Expr(..))
import Language.PureScript.SSA.Graph

data S = S { stateN            :: Integer
           , stateBlocks       :: Map BlockID Block
           , stateCurrentBlock :: BlockID
           }

type M = State S

fresh :: M Integer
fresh = do
  x <- gets stateN
  modify $ \s -> s { stateN = x + 1 }
  return x

tellInst :: Inst -> M Value
tellInst i = do
  iID <- InstID <$> fresh

  oldBlocks <- gets stateBlocks
  currentBlock <- gets stateCurrentBlock
  let newBlocks = Map.update (Just . (++ [(iID, i)])) currentBlock oldBlocks

  modify $ \s -> s { stateBlocks = newBlocks }
  return $ InstValue iID

fromExpr :: Expr Ann -> M Value
fromExpr (Literal _ (NumericLiteral (Left  n))) = return $ IntValue n
fromExpr (Literal _ (NumericLiteral (Right n))) = return $ DoubleValue n
fromExpr (Literal _ (StringLiteral  n))         = return $ StringValue n
fromExpr (Literal _ (CharLiteral    n))         = return $ CharValue n
fromExpr (Literal _ (BooleanLiteral n))         = return $ BooleanValue n
fromExpr (Literal _ (ArrayLiteral   n))         =
  mapM fromExpr n >>= tellInst . ArrayInst
fromExpr (Literal _ (ObjectLiteral  n))         =
  mapM (\(p, e) -> (p,) <$> fromExpr e) n >>= tellInst . ObjectInst
