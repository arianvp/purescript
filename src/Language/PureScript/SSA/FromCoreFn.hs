{-# LANGUAGE TupleSections #-}
module Language.PureScript.SSA.FromCoreFn
  (
  ) where

import Control.Monad.State (evalState, gets, modify, State)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Bind(..), Expr(..))
import Language.PureScript.Names (Qualified(..), runIdent)
import Language.PureScript.SSA.Graph

data S = S { stateN            :: Integer
           , stateBlocks       :: Map BlockID Block
           , stateCurrentBlock :: BlockID
           , stateVariables    :: Map String Value
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
-- TODO: construction???
fromExpr (Accessor _ p v) = fromExpr v >>= tellInst . AccessInst p
fromExpr (ObjectUpdate _ o ps) = do
  o' <- fromExpr o
  ps' <- mapM (\(p, e) -> (p,) <$> fromExpr e) ps
  tellInst $ ObjectUpdateInst o' ps'
-- TODO: lambdas???
fromExpr (App _ f a) = do
  f' <- fromExpr f
  a' <- fromExpr a
  tellInst $ CallInst f' [a']
-- TODO: qualified names???
fromExpr (Var _ (Qualified Nothing name)) =
  gets $ fromJust . Map.lookup (runIdent name) . stateVariables
-- TODO: cases???
fromExpr (Let _ bind value) = do
  mapM fromLocalBind bind
  fromExpr value

fromLocalBind :: Bind Ann -> M ()
fromLocalBind (NonRec _ name value) = do
  v <- fromExpr value
  oldVariables <- gets stateVariables
  let newVariables = Map.insert (runIdent name) v oldVariables
  modify $ \s -> s { stateVariables = newVariables }
-- TODO: Rec???
