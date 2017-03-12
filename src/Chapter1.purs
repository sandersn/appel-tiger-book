module Chapter1 where
import Prelude
import Data.Array (length)

type Id = String
data BinOp = Plus | Minus | Times | Div
data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm (Array Exp)
data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp
-- type Binding = 
-- type ExecState = { env :: Array { }, value :: Int }
maxargs :: Stm -> Int
maxargs (CompoundStm stm rest) = max (maxargs stm) (maxargs rest)
maxargs (AssignStm _ exp) = maxargs' exp
maxargs (PrintStm stms) = length stms

maxargs' :: Exp -> Int
maxargs' (EseqExp stm _) = maxargs stm
maxargs' _ = 0

-- it returns an int for now until I understand Purescript's effects
-- interp :: Stm -> Int
-- interp = (exec empty).value
-- of course this should use the equivalent of the state monad
-- exec (CompoundStm { stm, rest }) env = exec rest (exec stm).value
  

prog :: Stm
prog = CompoundStm
       (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
       (CompoundStm
        (AssignStm "b" (EseqExp
                        (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                        (OpExp (NumExp 10) Times (IdExp "a"))))
        (PrintStm [IdExp "b"]))
