{-# OPTIONS_GHC -fno-warn-orphans #-}

module L03.State where

import L01.Optional
import L02.List
import L03.Fluffy
import L03.Misty
import Data.Char
import qualified Data.Set as S
import qualified Data.Foldable as F

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- Exercise 1
-- Relative Difficulty: 2
-- Implement the `Fluffy` instance for `State s`.
instance Fluffy (State s) where
  furry f (State m) =
    State $ \s -> let (x, s') = m s in (f x, s')

-- Exercise 2
-- Relative Difficulty: 3
-- Implement the `Misty` instance for `State s`.
-- Make sure the state value is passed through in `banana`.
instance Misty (State s) where
  banana f (State m) =
    State $ \s -> let (x, s') = m s in runState (f x) s'
  unicorn x =
    State $ \s -> (x, s)

-- Exercise 3
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting state.
exec ::
  State s a
  -> s
  -> s
exec =
  (snd .) . runState

-- Exercise 4
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting state.
eval ::
  State s a
  -> s
  -> a
eval =
  (fst .) . runState

-- Exercise 5
-- Relative Difficulty: 2
-- A `State` where the state also distributes into the produced value.
get ::
  State s s
get =
  State $ \s -> (s, s)

-- Exercise 6
-- Relative Difficulty: 2
-- A `State` where the resulting state is seeded with the given value.
put ::
  s
  -> State s ()
put s =
  State $ const ((), s)

data Iter a = Skip | Yield a | Found a

iterM :: Misty f => List (f (Iter a)) -> f (List a)
iterM =
  foldRight (\mx -> flip banana mx . go) (unicorn Nil)
  where
    go mxs it = case it of
      Skip    -> mxs
      Yield x -> banana (unicorn . (x :|)) mxs
      Found x  -> unicorn (x :| Nil)

listToOptM :: Misty f => f (List a) -> f (Optional a)
listToOptM = furry' (flip headOr Empty . furry' Full)

-- Exercise 7
-- Relative Difficulty: 5
-- This function finds the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Misty` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
findM ::
  Misty f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM f = listToOptM . iterM . maap go
  where
    go x = banana (\b -> if b then unicorn (Found x) else unicorn Skip) (f x)

-- Exercise 8
-- Relative Difficulty: 4
-- This function finds the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
-- ~~~ Use findM and State with a Data.Set#Set. ~~~
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat =
  flip eval S.empty . findM (flip banana get . go)
  where
    go x seen =
      banana (const $ unicorn (S.member x seen)) $ put (S.insert x seen)

-- Exercise 9
-- Relative Difficulty: 5
-- This function removes all elements in a `List` that fail a given predicate.
-- However, while performing the filter, we sequence some `Misty` effect through.
--
-- Note the similarity of the type signature to List#filter
-- where the effect appears in every return position:
--   filter ::  (a ->   Bool) -> List a ->    List a
--   filterM :: (a -> f Bool) -> List a -> f (List a)
filterM ::
  Misty f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filterM p =
  iterM . maap go
  where
    go x = banana (\b -> unicorn $ if b then Yield x else Skip) (p x)

-- Exercise 10
-- Relative Difficulty: 4
-- This function removes all duplicate elements in a `List`.
-- ~~~ Use filterM and State with a Data.Set#Set. ~~~
distinct ::
  Ord a =>
  List a
  -> List a
distinct =
  flip eval S.empty . filterM (flip banana get . go)
  where
    go x seen =
      banana (const $ unicorn (S.notMember x seen)) $ put (S.insert x seen)

-- Exercise 11
-- Relative Difficulty: 3
-- This function produces an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
--
-- Should this go into src/L02/List.hs?  Can't see anything Misty or State
-- related about it
produce ::
  (a -> a)
  -> a
  -> List a
produce f x =
  x :| produce f (f x)

-- Exercise 12
-- Relative Difficulty: 10
-- A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
-- ~~~ Use findM with State and produce
-- ~~~ Use jellybean to write a square function
-- ~~~ Use library functions: Data.Foldable#elem, Data.Char#digitToInt
isHappy ::
  Integer
  -> Bool
isHappy =
  error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance F.Foldable Optional where
  foldr _ z Empty = z
  foldr f z (Full a) = f a z
