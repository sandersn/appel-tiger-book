module Main where

import Typescript (Node(..), parseR)
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.ST (ST, newSTRef, readSTRef, pureST, modifySTRef)
import Data.Array (length)
statements :: Node -> Int
statements (SourceFile ss) = length ss
statements _ = 0
main :: forall e ques. Eff (console :: CONSOLE, st :: ST ques | e) Unit
main = do
  logShow (encap 12)
  logShow (statements $ parseR "let s = 'roundtrip'; console.log(s)")
encap :: Int -> Int
encap n = pureST do
  r <- newSTRef n
  modifySTRef r (_ + 1)
  readSTRef r
