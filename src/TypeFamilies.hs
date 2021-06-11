{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module TypeFamilies where

import qualified Data.ByteString as B

import Data.Functor.Identity (Identity)
import Data.Word (Word8)
import GHC.TypeLits
    ( KnownSymbol, Nat, Symbol, type (+), type (-), symbolVal )
import Data.Proxy (Proxy(..))
import Type.Reflection (Typeable, typeRep, (:~:)(Refl))
import Data.Maybe (maybeToList)

-- Closed type families

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
-- it's commented here because I redefine it later
-- type family Not x where
--   Not True  = False
--   Not False = True

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


-- Type constructor arity

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


-- The synergy with GADTs

type HList :: [*] -> *
data HList xs where
  HNil :: HList '[]
  (:&) :: x -> HList xs -> HList (x : xs)
infixr 5 :&

h1 :: HList [Integer, String, Bool]
h1 = 42 :& "Hello" :& True :& HNil

h2 :: HList [Char, Bool]
h2 = 'x' :& False :& HNil

hlength :: HList xs -> Int
hlength HNil = 0
hlength (_ :& xs) = 1 + hlength xs

-- happend :: HList xs -> HList ys -> HList (Append xs ys)


-- Evaluation order, or lack thereof

type IteratePlus5 :: Nat -> [Nat]
type family IteratePlus5 k where
  IteratePlus5 k = k : IteratePlus5 (k + 5)

type Take :: Nat -> [a] -> [a]
type family Take n a where
  Take 0 xs = '[]
  Take n (x : xs) = x : Take (n - 1) xs


-- Open type families

type Label :: * -> Symbol
type family Label t

type instance Label Double = "number"
type instance Label String = "string"
type instance Label Bool   = "boolean"

label :: forall t. KnownSymbol (Label t) => String
label = symbolVal (Proxy @(Label t))

data MyType = MT
type instance Label MyType = "mt"

-- Overlapping equations

type And :: Bool -> Bool -> Bool
type family And a b where
  And True True = True
  And _    _    = False

type And' :: Bool -> Bool -> Bool
type family And' a b

type instance And' True  True  = True
type instance And' True  False = False
type instance And' False True  = False
type instance And' False False = False


-- Compatible equations

type family F a
type instance F a = [a]
type instance F Char = String

type family G a b
type instance G a    Bool = a -> Bool
type instance G Char b    = Char -> b
type instance G Char Bool = Char -> Bool

type family FInteger a where
  FInteger Char = Integer
  FInteger a    = [a]

type family FString a where
  FString Char = String
  FString a    = [a]

-- Injective type families

-- type Not :: Bool -> Bool
-- type family Not x where
--   Not True  = False
--   Not False = True

s :: forall x. (Not x ~ True, Typeable x) => String
s = show (typeRep @x)

type family Not x = r | r -> x where
  Not True  = False
  Not False = True

-- not_lemma :: Not x :~: True -> x :~: False
-- not_lemma Refl = Refl
-- Could not deduce: x ~ 'False
--   from the context: 'True ~ Not x
--     bound by a pattern with constructor:
--                Refl :: forall k (a :: k). a :~: a,
--              in an equation for ‘not_lemma’


-- Associated types

-- class Container a where
--   type Elem a
--   elements :: a -> [Elem a]

-- instance Container [a] where
--   type Elem [a] = a
--   elements = id

-- instance Container B.ByteString where
--   type Elem B.ByteString = Word8
--   elements = B.unpack

type family Unwrap x where
  Unwrap (f a) = a

class Container a where
  type Elem a
  type Elem x = Unwrap x
  elements :: a -> [Elem a]

instance Container [a] where
  elements = id

instance Container (Maybe a) where
  elements = maybeToList

instance Container B.ByteString where
  type Elem B.ByteString = Word8
  elements = B.unpack


-- Data families

data family Vector a
newtype instance Vector () = VUnit Int
newtype instance Vector Word8 = VBytes B.ByteString
data instance Vector (a, b) = VPair !(Vector a) !(Vector b)

type family VectorF a

type instance VectorF () = VectorUnit
data VectorUnit = VFUnit Int

type instance VectorF Word8 = VectorWord8
data VectorWord8 = VFBytes B.ByteString

type instance VectorF (a, b) = VectorPair a b
data VectorPair a b = VFPair (VectorF a) (VectorF b)

data Pair1 f x = P1 (f x) (f x)
type VV = Pair1 Vector
-- type VV = Pair1 VectorF -- not valid

class Vectorizable a where
  data VectorA a
  vlength :: Vector a -> Int


-- Non-parametric quantification

id' :: forall a. a -> a
-- id' (x :: Int) = 42 -- not valid
id' x = x

type C :: forall a. a -> a
type family C a where
  C (a :: Nat) = 42
  C a = a


-- Non-linear patterns

-- dedup (x : x : xs) = dedup (x : xs) -- not valid
dedup (x1 : x2 : xs) | x1 == x2 = dedup (x1 : xs)
dedup (y : xs) = y: dedup xs
dedup [] = []

type family Dedup xs where
  Dedup (x : x : xs) = Dedup (x : xs)
  Dedup (y : xs) = y : Dedup xs
  Dedup '[] = '[]
