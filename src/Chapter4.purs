module Chapter4 where
import Data.Tuple (Tuple)
import Data.Maybe (Maybe)

type Pos = Int
type Symbol = String -- maybe Tuple String Int (with counter) someday
data Var = SimpleVar Symbol -- x
    | FieldVar Var Symbol -- x.prop
    | SubscriptVar Var Exp -- x[prop]
data Exp = VarExp Var
         | NilExp
         | IntExp Int
         | StringExp String
         | CallExp { func :: Symbol, args :: Array Exp }
         | OpExp { left :: Exp, oper :: Oper, right :: Exp }
         | RecordExp { fields :: Array (Tuple Symbol Exp), typ :: Symbol }
         | SeqExp (Array Exp)
         | AssignExp { var :: Var, exp :: Exp }
         | IfExp { test :: Exp, then' :: Exp, else' :: Maybe Exp }
         | WhileExp { test :: Exp, body :: Exp }
         | ForExp { var :: Symbol, escape :: Boolean, lo :: Exp, hi :: Exp, body :: Exp }
         | BreakExp
         | LetExp { decs :: Array Dec, body :: Exp }
         | ArrayExp { typ :: Symbol, size :: Exp, init :: Exp }
data Oper = PlusOp | MinusOp | TimesOp | DivideOp
          |  EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
data Dec = FunctionDec (Array FunDec)
         | VarDec { name :: Symbol, escape :: Boolean, typ :: Maybe Symbol, init :: Exp }
         | TypeDec (Array { name :: Symbol, ty :: Ty })
data Ty = NameTy Symbol
        | RecordTy (Array Field)
        | ArrayTy Symbol
type Field = { name :: Symbol, escape :: Boolean, yyp :: Symbol }
type FunDec = { name :: Symbol, params :: Array Field, result :: Maybe Symbol, body :: Exp }
