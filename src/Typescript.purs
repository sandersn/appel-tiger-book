module Typescript where
import Prelude (bind, pure, ($), (<<<), (>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Except (runExcept)

import Data.Foreign (F, Foreign, readArray, readBoolean, readNumber, readString, readInt) -- , readNullOrUndefined)
import Data.Foreign.Index ((!))
-- import Data.Traversable (traverse)
import Data.Either (Either(..))
import Chapter4

foreign import parse :: String -> Foreign

parseR :: String -> Dec
parseR = fromEither (FunctionDec []) <<< runExcept <<< readDec <<< parse
readDec :: Foreign -> F Dec

readDec o = do
  kind <- o ! "kind" >>= readInt
  readDec' kind o
readDec' 264 o = readSourceFile o
readDec' 207 o = pure $ FunctionDec []
readDec' 209 o = pure $ FunctionDec []
readDec' 227 o = readFunction o
readDec' _ _ = pure $ FunctionDec []

fromEither :: forall a b. b -> Either a b -> b
fromEither _ (Right x) = x
fromEither y _ = y

-- currently it looks like all source files need to end in Dec
-- (although this is probably wrong and will require the AST to be extended)
readSourceFile :: Foreign -> F Dec
readSourceFile o = do
  stmts <- o ! "statements" >>= readArray
  badDefault stmts
badDefault :: Array Foreign -> F Dec
badDefault [x] = readDec x
badDefault _ = pure $ FunctionDec []
readFunction :: Foreign -> F Dec
readFunction o = do
  pure $ FunctionDec [] -- ha ha. very fake.
                
    

