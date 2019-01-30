module Data.Record.Scoped where

import Prelude
import Prim hiding (Record)

import Data.Array.Partial (head, tail)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons)
import Record.Unsafe (unsafeGet, unsafeSet)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Record :: # Type -> Type

foreign import ensureStack :: forall a. Array a -> Array a

empty :: Record ()
empty = unsafeCoerce {}

get :: forall r r' l a. IsSymbol l => Cons l a r' r => SProxy l -> Record r -> a
get l r =
    unsafePartial head stack
  where
    stack = unsafeGet (reflectSymbol l) (unsafeCoerce r)

set :: forall r1 r2 r l a b. IsSymbol l => Cons l a r r1 => Cons l b r r2 => SProxy l -> b -> Record r1 -> Record r2
set l b r =
    unsafeCoerce (unsafeSet (reflectSymbol l) stack' (unsafeCoerce r))
  where
    stack = unsafeGet (reflectSymbol l) (unsafeCoerce r)
    stack' = [b] <> unsafePartial tail stack

modify :: forall r1 r2 r l a b. IsSymbol l => Cons l a r r1 => Cons l b r r2 => SProxy l -> (a -> b) -> Record r1 -> Record r2
modify l f r = set l (f (get l r)) r

insert :: forall r1 r2 l a. IsSymbol l => Cons l a r1 r2 => SProxy l -> a -> Record r1 -> Record r2
insert l a r =
    unsafeCoerce (unsafeSet (reflectSymbol l) stack' (unsafeCoerce r))
  where
    stack = unsafeGet (reflectSymbol l) (unsafeCoerce r)
    stack' = [a] <> ensureStack stack

delete :: forall r1 r2 l a. IsSymbol l => Cons l a r1 r2 => SProxy l -> Record r2 -> Record r1
delete l r =
    unsafeCoerce (unsafeSet (reflectSymbol l) stack' (unsafeCoerce r))
  where
    stack = unsafeGet (reflectSymbol l) (unsafeCoerce r)
    stack' = unsafePartial tail stack
