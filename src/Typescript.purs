module Typescript where
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Except (runExcept)

import Data.Foreign (F, Foreign, readArray, readBoolean, readNumber, readString, readInt) -- , readNullOrUndefined)
import Data.Foreign.Index ((!))
import Data.Traversable (traverse)
import Data.Either (Either(..))

foreign import parse :: String -> Foreign

data Node = SourceFile (Array Node)
          | VariableStatement
          | ExpressionStatement
          | Statement

parseR :: String -> Node
parseR = fromEither (SourceFile []) <<< runExcept <<< readNode <<< parse
readNode :: Foreign -> F Node

readNode o = do
  kind <- o ! "kind" >>= readInt
  readNode' kind o
readNode' 264 o = readSourceFile o
readNode' 207 o = pure Statement
readNode' 209 o = pure Statement
readNode' _ _ = pure Statement

fromEither :: forall a b. b -> Either a b -> b
fromEither _ (Right x) = x
fromEither y _ = y

readSourceFile :: Foreign -> F Node
readSourceFile o = do
  stmts <- o ! "statements" >>= readArray >>= traverse readNode
  pure $ SourceFile stmts
readStatement :: Foreign -> F Node
readStatement o = do
  pure $ Statement -- ha ha. very fake.
                
    

