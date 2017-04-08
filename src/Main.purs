module Main where

import Typescript (parse)
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.ST (ST, newSTRef, readSTRef, pureST, modifySTRef)
main :: forall e ques. Eff (console :: CONSOLE, st :: ST ques | e) Unit
main = do
  logShow (encap 12)
  log (parse "let s = 'roundtrip'")
encap :: Int -> Int
encap n = pureST do
  r <- newSTRef n
  modifySTRef r (_ + 1)
  readSTRef r
