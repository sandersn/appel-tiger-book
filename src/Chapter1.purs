module Chapter1 where
import Prelude
import Data.Array (length)

type Id = String
data BinOp = Plus | Minus | Times | Div
data Stm = CompoundStm { stm :: Stm, rest :: Stm }
         | AssignStm { id :: Id, exp :: Exp }
         | PrintStm (Array Exp)
data Exp = IdExp Id
         | NumExp Int
         | OpExp { l :: Exp, op :: BinOp, r :: Exp }
         | EseqExp { stm :: Stm, rest :: Exp }
prog :: Stm
prog = (CompoundStm { stm: AssignStm { id: "a"
                                     , exp: OpExp { l: NumExp 5
                                                  , op: Plus
                                                  , r: NumExp 3
                                                  }
                                     }
                    , rest: (CompoundStm { stm: AssignStm { id: "b"
                                                          , exp: EseqExp { stm: PrintStm [IdExp "a"
                                                                                         , OpExp { l: IdExp "a"
                                                                                                 , op: Minus
                                                                                                 , r: NumExp 1 }]
                                                                         , rest: OpExp { l: NumExp 10, op: Times, r: IdExp "a" } } },
                                           rest: PrintStm [IdExp "b"] }) })
maxargs :: Stm -> Int
maxargs (CompoundStm { stm, rest }) = max (maxargs stm) (maxargs rest)
maxargs (AssignStm { exp }) = maxargs' exp
maxargs (PrintStm stms) = length stms

maxargs' :: Exp -> Int
maxargs' (EseqExp { stm }) = maxargs stm
maxargs' _ = 0
