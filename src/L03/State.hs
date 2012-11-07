{-# OPTIONS_GHC -fno-warn-orphans #-}

module L03.State where

import L01.Optional
import L02.List
import L03.Fluffy
import L03.Misty
import Data.Char
import qualified Data.Set as S
import qualified Data.Foldable as F

newtype State s a =
  State {
    runState :: s -> (a, s)
  }

instance Fluffy (State s) where
  furry f (State k) =
    State (\s -> let (a, t) = k s in (f a, t))

instance Misty (State s) where
  banana f (State k) =
    State (\s -> let (a, t) = k s in runState (f a) t)
  unicorn a =
    State (\s -> (a, s))

exec ::
  State s a
  -> s
  -> s
exec (State k) =
  snd . k

eval ::
  State s a
  -> s
  -> a
eval (State k) =
  fst . k

get ::
  State s s
get =
  State (\s -> (s, s))

put ::
  s
  -> State s ()
put =
  State . const . (,) ()

findM ::
  Misty f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil =
  unicorn Empty
findM p (h :| t) =
  banana (\q -> if q then unicorn (Full h) else findM p t) (p h)

firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat x =
  eval (findM (\a -> State (\s -> (a `S.member` s, a `S.insert` s))) x) S.empty

instance F.Foldable Optional where
  foldr _ z Empty = z
  foldr f z (Full a) = f a z

filterM ::
  Misty f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filterM _ Nil =
  unicorn Nil
filterM p (h :| t) =
 banana (\q -> furry' (if q
                         then
                           (h:|)
                         else
                           id) (filterM p t)) (p h)

distinct ::
  Ord a =>
  List a
  -> List a
distinct x =
  eval (filterM (\a -> State (\s -> (a `S.notMember` s, a `S.insert` s))) x) S.empty

produce ::
  (a -> a)
  -> a
  -> List a
produce f a =
  a :| produce f (f a)

isHappy ::
  Integer
  -> Bool
isHappy =
  F.elem 1 .
    (`eval` S.empty) .
    findM (\j -> State $ \s -> (j == 1 || S.member j s, S.insert j s)) .
    produce (sum .
             map (jellybean (*) .
                  toInteger .
                  digitToInt) .
             show)

