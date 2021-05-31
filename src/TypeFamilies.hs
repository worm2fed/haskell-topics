{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypeFamilies where

import Data.Functor.Identity (Identity)

append :: forall a. [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
  Append '[]    ys = ys
  Append (x:xs) ys = x : Append xs ys

not :: Bool -> Bool
not True = False
not False = True

-- arity 1
type Not :: Bool -> Bool
type family Not a where
  Not True  = False
  Not False = True

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing  = d
fromMaybe _ (Just x) = x

-- arity 2
type FromMaybe :: a -> Maybe a -> a
type family FromMaybe d x where
  FromMaybe d Nothing  = d
  FromMaybe _ (Just x) = x

fst :: (a, b) -> a
fst (x, _) = x

-- arity 2
type Fst :: (a, b) -> a
type family Fst t where
  Fst '(x, _) = x


type S :: (* -> *) -> *
data S k = MkS (k Bool) (k Integer)

type Pair :: * -> *
type Pair a = (a, a)

type MaybeIf :: Bool -> * -> *
type family MaybeIf b where
  MaybeIf True  = Maybe
  MaybeIf False = Identity

data PlayerInfo b = MkPlayerInfo
  { name  :: MaybeIf b String
  , score :: MaybeIf b Integer
  }

-- dbReadPlayerInfo :: IO (PlayerInfo False)
-- dbUpdatePlayerInfo :: PlayerInfo True -> IO ()

