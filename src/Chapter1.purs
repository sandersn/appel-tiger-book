module Chapter1 where
import Prelude
import Data.Traversable (for_)
import Data.Array (length)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, runST, modifySTRef)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

type Id = String
data BinOp = Plus | Minus | Times | Div
data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm (Array Exp)
data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp
type Environment = Map Id Int
type ExecState = { env :: Environment, value :: Int }
maxargs :: Stm -> Int
maxargs (CompoundStm stm rest) = max (maxargs stm) (maxargs rest)
maxargs (AssignStm _ exp) = maxargs' exp
maxargs (PrintStm stms) = length stms

maxargs' :: Exp -> Int
maxargs' (EseqExp stm _) = maxargs stm
maxargs' _ = 0

interp :: forall eff.Stm -> Eff (console :: CONSOLE | eff) Int
interp stm = runST do
  env <- newSTRef empty
  exec stm env
exec :: forall eff st.Stm -> STRef st (Map String Int) -> Eff (st :: ST st, console :: CONSOLE | eff) Int 
exec (CompoundStm stm rest) env = do
  exec stm env
  exec rest env
exec (AssignStm id exp) env = do
  value <- eval exp env
  modifySTRef env (insert id value)
  pure value
exec (PrintStm exps) env = do
  for_ exps \exp -> do
    value <- eval exp env
    logShow value
  pure (-1)

eval :: forall eff st.Exp -> STRef st (Map String Int) -> Eff (st :: ST st, console :: CONSOLE | eff) Int 
eval (IdExp id) env = do
  d <- readSTRef env
  pure $ fromMaybe (-1) (lookup id d)
eval (NumExp n) env = pure n
eval (OpExp l op r) env = do
  lval <- eval l env
  rval <- eval r env
  pure $ math lval op rval
eval (EseqExp stm exp) env = do
  exec stm env
  eval exp env
math :: forall n.(Semiring n, Ring n, EuclideanRing n) => n -> BinOp -> n -> n
math l Plus r = l + r
math l Minus r = l - r
math l Times r = l * r
math l Div r = l / r

prog :: Stm
prog = CompoundStm
       (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
       (CompoundStm
        (AssignStm "b" (EseqExp
                        (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                        (OpExp (NumExp 10) Times (IdExp "a"))))
        (PrintStm [IdExp "b"]))
