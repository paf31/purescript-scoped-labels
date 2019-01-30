module Test.Main where

import Prelude
import Data.Symbol (SProxy(..))
import Data.Record.Scoped (delete, empty, get, insert, modify, set)
import Effect (Effect)
import Test.Assert (assert')

foo :: SProxy "foo"
foo = SProxy

main :: Effect Unit
main = do
  assert' "insert, get" $
    get foo (insert foo 42 empty) == 42
  assert' "insert, insert, get" $
    get foo (insert foo 42 (insert foo true empty)) == 42
  assert' "insert, modify, get" $
    get foo (modify foo (_ + 1) (insert foo 42 empty)) == 43
  assert' "set, get" $
    get foo (set foo 43 (insert foo 42 empty)) == 43
  assert' "set, modify, get" $
    get foo (modify foo (_ + 1) (set foo 0 (insert foo 42 empty))) == 1
  assert' "insert, insert, delete, get" $
    get foo (delete foo (insert foo 42 (insert foo true empty))) == true
