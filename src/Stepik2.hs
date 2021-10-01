{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE TypeOperators #-}

module Stepik2 where

-- import Text.Parsec (Parsec, char, digit, many1, sepBy)
import Control.Applicative (ZipList(..), (<**>), Alternative(empty, (<|>)))
import Data.Char (isDigit)
import Text.Parsec (Parsec)
import Data.Monoid (Any(..), All (..), Endo(..), Last (..))


-- Applicative Functors


-- succ <$> "abc" :: (Char -> Char) -> [Char] -> [Char]
-- (<$>) :: (a -> a) -> f a -> f a
--
-- succ <$> succ <$> "abc"
-- ((Char -> Char) -> (Char -> Char) -> Char -> Char) -> [Char] -> [Char]

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 g) = Arr2 $ \e1 e2 -> f $ g e1 e2

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 g) = Arr3 $ \e1 e2 e3 -> f $ g e1 e2 e3

-- 1. fmap id cont == cont
-- 2. fmap f (fmap g cont) == fmap (f . g) cont

-- instance Functor ((->) e) where
--   -- fmap :: (a -> b) -> (e -> a) -> (e -> b)
--   fmap = (.)

-- fmap id e = id . e = e
-- fmap f (fmap g e) = fmap f (g . e) = f . (g . e) = (f . g) . e =
--   fmap (f . g) e

-- map _ [] = []
-- map f (x : xs) = f x : map f xs

-- instance Functor [] where
--   fmap = map

-- Base case:
-- fmap f (fmap g [])
--  == fmap f []
--  == []
-- fmap (f . g) []
--  == []
-- Induction step:
-- fmap f (fmap g (x : xs))
--  == fmap f (g x : fmap g xs)
--  == fmap f gxs -- gxs is list where g applied to all elements
--  == fmap f (gx : gxs)
--  == f gx : fmap f gxs
--  == fgxs -- fgxs is list where g and f applied to all elements
-- fmap (f . g) (x : xs)
--  == (f . g $ x) : fmap (f . g) xs
--  == f gx : fmap (f . g) xs
--  == fgx : fmap (f . g) xs
--  == fgxs

data Triple a = Tr a a a
  deriving (Eq, Show)

instance Functor Triple where
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  pure a = Tr a a a
  Tr a1 b1 c1 <*> Tr a2 b2 c2 = Tr (a1 a2) (b1 b2) (c1 c2)

infixl 4 >$<
(>$<) :: (a -> b) -> [a] -> [b]
f >$< e = getZipList $ f <$> ZipList e

infixl 4 >*<
(>*<) :: [a -> b] -> [a] -> [b]
a >*< b = getZipList $ ZipList a <*> ZipList b

divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)

divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> divideList' xs

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 $ \e1 e2 -> x
  (Arr2 g) <*> (Arr2 h) = Arr2 $ \e1 e2 -> g e1 e2 $ h e1 e2

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 $ \e1 e2 e3 -> x
  (Arr3 g) <*> (Arr3 h) = Arr3 $ \e1 e2 e3 -> g e1 e2 e3 $ h e1 e2 e3

infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)

exprMaybe :: (forall a b . Maybe a -> Maybe (a -> b) -> Maybe b) -> Maybe Int
exprMaybe op =
  let (<??>) = op
      infixl 4 <??>
  in Just 5 <??> Just (+2)

exprList :: (forall a b . [a] -> [a -> b] -> [b]) -> [Int]
exprList op =
  let (<??>) = op
      infixl 4 <??>
  in [1, 2, 0] <??> [(+3), (+4)]

exprZipList :: (forall a b . ZipList a -> ZipList (a -> b) -> ZipList b) -> ZipList Int
exprZipList op =
  let (<??>) = op
      infixl 4 <??>
  in ZipList [1, 2] <??> ZipList [(+3), (+4)]

exprEither
  :: (forall a b . Either String a -> Either String (a -> b) -> Either String b)
  -> Either String Int
exprEither op =
  let (<??>) = op
      infixl 4 <??>
  in Left "AA" <??> Left "BB"

exprPair :: (forall a b . (String, a) -> (String, a -> b) -> (String, b)) -> (String, Int)
exprPair op =
  let (<??>) = op
      infixl 4 <??>
  in ("AA", 3) <??> ("BB", (+1))

exprEnv
  :: (forall a b . (String -> a) -> (String -> (a -> b)) -> (String -> b))
  -> (String -> Int)
exprEnv op =
  let (<??>) = op
      infixl 4 <??>
  in length <??> (\_ -> (+5))

-- getList :: Parsec String u [String]
-- getList = many1 digit `sepBy` char ';'

ignoreBraces
  :: Parsec [Char] u a
  -> Parsec [Char] u b
  -> Parsec [Char] u c
  -> Parsec [Char] u c
ignoreBraces open close inside = open *> inside <* close

newtype Prs a = Prs
  { runPrs :: String -> Maybe (a, String)
  }

anyChr :: Prs Char
anyChr = Prs $ \case
  "" -> Nothing
  (c:cs) -> Just (c, cs)

instance Functor Prs where
  fmap f (Prs p) = Prs $ fmap (\(a, b) -> (f a, b)) . p

instance Applicative Prs where
  pure a = Prs $ \s -> Just (a, s)
  Prs f <*> Prs p = Prs $ \s ->
    case f s of
      Nothing -> Nothing
      Just (g, s') ->
        (\(a, s'') -> (g a, s'')) <$> p s'

newtype PrsE a = PrsE
  { runPrsE :: String -> Either String (a, String)
  }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE $ \case
  "" -> Left "unexpected end of input"
  (x:xs) ->
    if p x
    then Right (x, xs)
    else Left $ "unexpected " ++ [x]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

instance Functor PrsE where
  fmap f (PrsE p) = PrsE $ fmap (\(a, b) -> (f a, b)) . p

instance Applicative PrsE where
  pure a = PrsE $ \s -> Right (a, s)
  PrsE f <*> PrsE p = PrsE $ \s ->
    case f s of
      Left e -> Left e
      Right (g, s') ->
        (\(a, s'') -> (g a, s'')) <$> p s'

char :: Char -> Prs Char
char ch = Prs $ \case
  "" -> Nothing
  (c:cs)
    | ch == c -> Just (c, cs)
    | otherwise -> Nothing

instance Alternative Prs where
  empty = Prs $ \s -> Nothing
  Prs f <|> Prs p = Prs $ \s ->
    case f s of
      Nothing -> p s
      Just v -> Just v

many :: Prs a -> Prs [a]
many p = (:) <$> p <*> many p <|> pure []

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> many p

digit :: Prs Char
digit = Prs $ \case
  "" -> Nothing
  (c:cs)
    | isDigit c -> Just (c, cs)
    | otherwise -> Nothing

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

nat :: Prs Int
nat = read <$> many1 digit

infixr 9 |.|
newtype (|.|) f g a = Cmps
  { getCmps :: f (g a)
  } deriving (Show, Eq)

type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (4, ('2', True))

b :: B t
b = Cmps (False, id, Right 42)

c :: C
c  = Cmps $ \_ _ -> 42

newtype Cmps3 f g h a = Cmps3
  { getCmps3 :: f (g (h a))
  } deriving (Show, Eq)

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap k (Cmps3 x) = Cmps3 $ fmap (fmap $ fmap k) x

-- fmap law
-- fmap f (fmap g x) == fmap (f . g) x
-- fmap f $ fmap g x == fmap (f . g) x
-- fmap f $ fmap g $ x == fmap (f . g) $ x
-- fmap f . fmap g $ x == fmap (f . g) $ x
-- fmap f . fmap g == fmap (f . g)
--
-- def fmap
-- fmap h (Cmps x) = Cmps $ fmap (fmap h) x
--
-- left part
-- fmap h2 (fmap h1 (Cmps x))
-- == fmap h2 (Cmps $ fmap (fmap h1) x)  -- def fmap
-- == Cmps $ fmap (fmap h2) (fmap (fmap h1) x)  -- def fmap
-- == Cmps $ fmap (fmap h2) $ fmap (fmap h1) x
-- == Cmps $ fmap (fmap h2) $ fmap (fmap h1) x
-- == Cmps $ fmap f $ fmap g x -- let f = fmap h2, let g = fmap h1
-- == Cmps (fmap f $ fmap g $ x)
-- == Cmps (fmap f . fmap g $ x)
-- == Cmps (fmap f . fmap g $ x)
-- == Cmps (fmap (f . g) x) -- fmap law
-- == Cmps (fmap (fmap h2 . fmap h1) x) -- reduct f and g
-- == Cmps $ fmap (fmap h2 . fmap h1) x
-- == Cmps $ fmap (fmap (h2 . h1)) x -- fmap law
--
-- right part
-- fmap (h2 . h1) (Cmps x)
-- == Cmps $ fmap (fmap (h2 . h1)) x

-- Cmps $ fmap (fmap (h2 . h1)) x == Cmps $ fmap (fmap (h2 . h1)) x

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps x) = Cmps $ fmap (fmap h) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
  Cmps h <*> Cmps x = Cmps $ fmap (<*>) h <*> x

unCmps3 :: (Functor f, Functor g, Functor h) => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = fmap getCmps . getCmps

unCmps4
  :: (Functor f2, Functor f1, Functor g, Functor h)
  => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = fmap (fmap getCmps . getCmps) . getCmps
