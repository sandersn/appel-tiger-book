module Test.Main where

import Chapter1 (maxargs, prog, interp)
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Assert (assert, ASSERT)

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  assert (maxargs prog == 2)
  val <- interp prog
  assert (val == -1)
