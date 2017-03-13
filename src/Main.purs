module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.ST (ST, newSTRef, readSTRef, runST, pureST, modifySTRef)
main :: forall e ques. Eff (console :: CONSOLE, st :: ST ques | e) Unit
main = do
  logShow (encap 12)
encap n = pureST do
  r <- newSTRef n
  modifySTRef r (_ + 1)
  readSTRef r
