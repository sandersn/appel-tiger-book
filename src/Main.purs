module Main where

import Typescript (parseR)
import Prelude (Unit, bind, ($), (+), (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.ST (ST, newSTRef, readSTRef, pureST, modifySTRef)
import Data.Array (length)
import Chapter4
statements :: Dec -> Int
statements (FunctionDec ss) = length ss
statements _ = 0
main :: forall e ques. Eff (console :: CONSOLE, st :: ST ques | e) Unit
main = do
  logShow (encap 12)
  logShow (statements $ parseR ("function f(a: int, b: int, c: int) {"
                                <> "print_int(a + c);"
                                <> "let j = a + b;"
                                <> "let a = 'hello';"
                                <> "print(a);"
                                <> "print_int(j);"
                                <> "print_int(b);"
                                <> "}"))
encap :: Int -> Int
encap n = pureST do
  r <- newSTRef n
  modifySTRef r (_ + 1)
  readSTRef r
