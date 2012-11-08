{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module L04.ListZipper where

import Data.List
import L03.Fluffy

data ListZipper a =
  ListZipper [a] a [a]
  deriving Eq

data MaybeListZipper a =
  IsZ (ListZipper a)
  | IsNotZ
  deriving Eq

instance Fluffy ListZipper where
  furry =
    error "todo"

instance Fluffy MaybeListZipper where
  furry =
    error "todo"

fromList ::
  [a]
  -> MaybeListZipper a
fromList =
  error "todo"

toMaybe ::
  MaybeListZipper a
  -> Maybe (ListZipper a)
toMaybe =
  error "todo"

class Fluffy f => ListZipper' f where
  toMaybeListZipper ::
    f a
    -> MaybeListZipper a
  fromListZipper ::
    ListZipper a
    -> f a

instance ListZipper' ListZipper where
  toMaybeListZipper =
    IsZ
  fromListZipper =
    id

instance ListZipper' MaybeListZipper where
  toMaybeListZipper =
    id
  fromListZipper =
    IsZ

toList ::
  ListZipper' f =>
  f a
  -> [a]
toList =
  error "todo"

withFocus ::
  ListZipper' f =>
  (a -> a)
  -> f a
  -> f a
withFocus =
  error "todo"

setFocus ::
  ListZipper' f =>
  a
  -> f a
  -> f a
setFocus =
  error "todo"

(.=) ::
  ListZipper' f =>
  f a
  -> a
  -> f a
(.=) =
  flip setFocus

hasLeft ::
  ListZipper' f =>
  f a
  -> Bool
hasLeft =
  error "todo"

hasRight ::
  ListZipper' f =>
  f a
  -> Bool
hasRight =
  error "todo"

findLeft ::
  ListZipper' f =>
  (a -> Bool)
  -> f a
  -> MaybeListZipper a
findLeft =
  error "todo"

findRight ::
  ListZipper' f =>
  (a -> Bool)
  -> f a
  -> MaybeListZipper a
findRight =
  error "todo"

moveRightLoop ::
  ListZipper' f =>
  f a
  -> f a
moveRightLoop =
  error "todo"

-- !! non-total
moveLeftLoop ::
  ListZipper' f =>
  f a
  -> f a
moveLeftLoop =
  error "todo"

moveRight ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
moveRight =
  error "todo"

moveLeft ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
moveLeft =
  error "todo"

swapRight ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
swapRight =
  error "todo"

swapLeft ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
swapLeft =
  error "todo"

dropRights ::
  ListZipper' f =>
  f a
  -> f a
dropRights =
  error "todo"

dropLefts ::
  ListZipper' f =>
  f a
  -> f a
dropLefts =
  error "todo"

moveLeftN ::
  ListZipper' f =>
  Int
  -> f a
  -> MaybeListZipper a
moveLeftN =
  error "todo"

moveRightN ::
  ListZipper' f =>
  Int
  -> f a
  -> MaybeListZipper a
moveRightN =
  error "todo"

moveLeftN' ::
  ListZipper' f =>
  Int
  -> f a
  -> Either Int (f a)
moveLeftN' =
  error "todo"

moveRightN' ::
  ListZipper' f =>
  Int
  -> f a
  -> Either Int (f a)
moveRightN' =
  error "todo"

-- non-total
nth ::
  ListZipper' f =>
  Int
  -> f a
  -> MaybeListZipper a
nth =
  error "todo"

mfocus ::
  ListZipper' f =>
  f a
  -> Maybe a
mfocus =
  error "todo"

index ::
  ListZipper' f =>
  f a
  -> Maybe Int
index =
  error "todo"

-- non-total
end ::
  ListZipper' f =>
  f a
  -> f a
end =
  error "todo"

start ::
  ListZipper' f =>
  f a
  -> f a
start =
  error "todo"

deletePullRight ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
deletePullRight =
  error "todo"

deletePullLeft ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
deletePullLeft =
  error "todo"

insertPushLeft ::
  ListZipper' f =>
  a
  -> f a
  -> f a
insertPushLeft =
  error "todo"

insertPushRight ::
  ListZipper' f =>
  a
  -> f a
  -> f a
insertPushRight =
  error "todo"

class Fluffy f => Apply f where
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

class Apply f => Applicative f where
  unit ::
    a -> f a

class Fluffy f => Extend f where
  (<<=) ::
    (f a -> b)
    -> f a
    -> f b

class Extend f => Comonad f where
  counit ::
    f a
    -> a

class Fluffy t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance Traversable [] where
  traverse =
    error "todo"

instance Apply ListZipper where
  (<*>) =
    error "todo"

instance Apply MaybeListZipper where
  (<*>) =
    error "todo"

instance Applicative ListZipper where
  unit =
    error "todo"

instance Applicative MaybeListZipper where
  unit =
    error "todo"

instance Extend ListZipper where
  (<<=) =
    error "todo"

instance Comonad ListZipper where
  counit =
    error "todo"

instance Traversable ListZipper where
  traverse =
    error "todo"

instance Traversable MaybeListZipper where
  traverse =
    error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Show a => Show (ListZipper a) where
  show (ListZipper l x r) =
    (show . reverse $ l) ++ ('⋙':show x ++ "⋘") ++ show r

instance Show a => Show (MaybeListZipper a) where
  show (IsZ z) = show z
  show IsNotZ = "∅"
