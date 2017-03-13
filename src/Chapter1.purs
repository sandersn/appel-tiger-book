module Chapter1 where
import Prelude
import Data.Array (length)
import Data.Map
import Data.Maybe (fromMaybe)
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, pureST, modifySTRef)

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

-- it returns an int for now until I understand Purescript's effects
interp :: Stm -> Int
interp stm = pureST do
  env <- newSTRef empty
  exec stm env
exec :: forall eff st.Stm -> STRef st (Map String Int) -> Eff (st :: ST st | eff) Int 
exec (CompoundStm stm rest) env = do
  exec stm env
  exec rest env
exec (AssignStm id exp) env = do
  d <- readSTRef env
  let value = eval exp d
  modifySTRef env (insert id value)
  pure value
exec (PrintStm exps) env = pure (-1) -- (-1) -- not implemented yet!

eval :: Exp -> Environment -> Int
eval (IdExp id) env = fromMaybe (-1) (lookup id env)
eval (NumExp n) env = n
eval (OpExp l op r) env = eval l env + eval r env -- TODO: Incomplete!
eval (EseqExp stm exp) env = eval exp empty -- uggggg I forgot about this one. What a dumb idea! Now I have to lift this into State Exec as well!
  

prog :: Stm
prog = CompoundStm
       (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
       (CompoundStm
        (AssignStm "b" (EseqExp
                        (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                        (OpExp (NumExp 10) Times (IdExp "a"))))
        (PrintStm [IdExp "b"]))
