module Test.Main where

import Chapter1 (maxargs, prog)
import Prelude ((==), Unit)
import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (assert, ASSERT)

main :: forall eff. Eff (assert :: ASSERT | eff) Unit
main = do
  assert (maxargs prog == 2)
