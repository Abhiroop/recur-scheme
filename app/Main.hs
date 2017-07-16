{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.Functor.Foldable hiding (ListF)
import Data.List.Ordered(merge)
import Prelude hiding (Foldable, succ)

import Lib

data Natural =
    Zero
  | Succ Natural

data NatF r =
    ZeroF
  | SuccF r
  deriving (Show, Functor)

data ListF a r =
    NilF
  | ConsF a r
  deriving (Show, Functor)

data TreeF a r =
    EmptyF
  | LeafF a
  | NodeF r r
  deriving (Show, Functor)

type Nat = Fix NatF
type List a = Fix (ListF a)
type Tree a = Fix (TreeF a)

zero :: Nat
zero = Fix ZeroF

succ :: Nat -> Nat
succ = Fix . SuccF

nil :: List a
nil = Fix NilF

cons :: a -> List a -> List a
cons x xs = Fix (ConsF x xs)

main :: IO ()
main = someFunc
