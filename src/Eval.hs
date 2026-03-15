{-# LANGUAGE InstanceSigs #-}

module Eval (
  runEval,
  TermEnv,
  Value(..),
  emptyTmenv
) where

import Syntax

import Control.Monad.Identity ( Identity(runIdentity) )
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure [(Pattern, Expr)] TermEnv
  -- TODO-1: Create a way to store arrays `VArray`
  | VArray [Value]
  -- TODO-2: Edit VClosure to store a list of (pattern, expression) tuples
  --         these represent a map from pattern to expression body

type TermEnv = Map.Map String Value
type Interpreter t = Identity t

instance MonadFail Identity where
  fail :: String -> Identity a
  fail = error

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

instance Show Value where
  show :: Value -> String
  show (VInt n) = show n
  show (VBool n) = show n
  show VClosure{} = "<<closure>>"
  -- TODO-1: Show VArr
  show (VArray xs) = show xs

-- TODO-2: add a checkeq function to compare literals and values
-- checkeq :: Lit -> Value -> Bool
checkeq :: Lit -> Value -> Bool
checkeq (LInt li) (VInt vi) = (li == vi)
checkeq (LBool lb) (VBool vb) = (lb == vb)
checkeq (LArray []) (VArray []) = True
checkeq _ _ = False 

-- TODO-2: Add a match function to handle pattern matching
-- match :: [(Pattern, Expr)] -> Value -> (Expr, TermEnv)
-- When matching against a pattern, you can check:
-- 1. Is the pattern a PVar? -> always match, this is the generic case
-- 2. Is the pattern a PLit? -> match if the argument is equivalent to the literal
-- 3. Is the pattern a (x:xs) structure? -> match if the argument is a non-empty list
-- 4. Otherwise, check another pattern
matchHelper :: Pattern -> Value -> Maybe TermEnv
matchHelper (PVar pv) val = Just (Map.singleton pv val)
matchHelper (PLit pl) val = 
  if checkeq pl val then Just Map.empty else Nothing
matchHelper (PCons x xs) (VArray (v:vs)) = do
  env1 <- matchHelper x v
  env2 <- matchHelper xs (VArray vs)
  return (Map.union env1 env2)
matchHelper (PCons _ _) _ = Nothing
matchHelper _ _ = Nothing

match :: [(Pattern, Expr)] -> Value -> (Expr, TermEnv)
match [] _ = error "Non-exhaustive patterns in function application"
match ((pat,exp):vc) val = 
  case (matchHelper pat val) of
    Just env -> (exp, env)
    Nothing -> match vc val

eval :: TermEnv -> Expr -> Interpreter Value
eval env expr = case expr of
  Lit (LInt k)  -> return $ VInt k
  Lit (LBool k) -> return $ VBool k
  -- TODO-1: Handle evaluating arrays
  -- Suggestion: Use a recursive call to evaluate each element one at a time
  Lit (LArray []) -> return $ VArray []
  Lit (LArray (x:xs)) -> do
    v <- eval env x
    VArray vs <- eval env (Lit (LArray xs))
    return $ VArray (v : vs)

  Var x -> do
    let Just v = Map.lookup x env
    return v

  -- TODO-1: Add the Cons Operator
  -- Suggestion: Create a separate handler for this case
  --             because Cons is not the same type as other binop
  Op Cons x xs -> do
    v <- eval env x
    VArray vs <- eval env xs
    return $ VArray (v : vs)

  -- TODO-2: Add the Concat Operator
  -- Suggestion: Create a separate handler for this case
  --             because Cons is not the same type as other binop
  Op Concat e1 e2 -> do
    VArray vs1 <- eval env e1
    VArray vs2 <- eval env e2
    return $ VArray (vs1 ++ vs2)

  Op op a b -> do
    VInt a' <- eval env a
    VInt b' <- eval env b
    return $ binop op a' b'

  Lam x body ->
    -- TODO-2: Change VClosure to store a list of patterns and expressions
    return (VClosure [(x, body)] env)

  App fun arg -> do
    -- TODO-2: Implement pattern matching in App
    VClosure pats cloEnv <- eval env fun
    argv <- eval env arg
    let (body, bindEnv) = match pats argv
    eval (Map.union bindEnv cloEnv) body

  Let x e body -> do
    e' <- eval env e
    let nenv = Map.insert x e' env
    eval nenv body

  If cond tr fl -> do
    VBool br <- eval env cond
    if br
    then eval env tr
    else eval env fl

  Fix e -> do
    eval env (App e (Fix e))

binop :: Binop -> Integer -> Integer -> Value
binop Add a b = VInt $ a + b
binop Mul a b = VInt $ a * b
binop Sub a b = VInt $ a - b
binop Eql a b = VBool $ a == b

-- TODO-2: Make sure that when you have a new definition for a function, you append the 
--         (pattern, body) to the environment instead of overwriting it
runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex =
  let res = runIdentity (eval env ex) in
    case (Map.lookup nm env, res) of
      (Just (VClosure oldPats oldEnv), VClosure newPats _) ->
          let combined = VClosure (oldPats ++ newPats) oldEnv
          in (combined, Map.insert nm combined env)
          -- Otherwise, just overwrite the definition
      _ -> (res, Map.insert nm res env)