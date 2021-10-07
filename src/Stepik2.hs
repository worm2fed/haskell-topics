{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE TypeOperators #-}

module Stepik2 where

-- import Text.Parsec (Parsec, char, digit, many1, sepBy)
import Control.Applicative (ZipList(..), (<**>), Alternative(empty, (<|>)))
import Control.Monad (ap, MonadPlus (mplus))
import Data.Char (isDigit)
import Data.Monoid (Any(..), All (..), Endo(..), Last (..))
import Data.Traversable (foldMapDefault)
import Text.Parsec (Parsec)


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


-- Managing Effects


-- data Triple a = Tr a a a
--   deriving (Eq, Show)

instance Foldable Triple where
  foldr f ini (Tr a b c) = f a $ f b $ f c ini
  foldl f ini (Tr a b c) = f (f (f ini a) b) c

data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

testTree :: Tree Int
testTree = Branch
  (Branch
    (Branch Nil 1 Nil)
    2
    (Branch Nil 3 Nil))
  4
  (Branch Nil 5 Nil)

-- instance Foldable Tree where
--   foldr f ini Nil = ini
--   foldr f ini (Branch l x r) =
--     foldr f (f x $ foldr f ini r) l

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

newtype Preorder a   = PreO   (Tree a)
  deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)
  deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)
  deriving (Eq, Show)

instance Foldable Preorder where
  foldr f ini (PreO Nil) = ini
  foldr f ini (PreO (Branch l x r)) =
    f x (foldr f (foldr f ini $ PreO r) $ PreO l)

instance Foldable Postorder where
  foldr f ini (PostO Nil) = ini
  foldr f ini (PostO (Branch l x r)) =
    foldr f (foldr f (f x ini) $ PostO r) $ PostO l

instance Foldable Levelorder where
  foldr f ini (LevelO tree) = go [tree]
    where
      go [] = ini
      go (Nil : xs) = go xs
      go (Branch l x r : xs) = f x $ go (xs ++ [l, r])

treeToList :: Foldable t => t a -> [a]
treeToList = foldr (:) []

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo

-- infixr 9 |.|
-- newtype (|.|) f g a = Cmps
--   { getCmps :: f (g a)
--   } deriving (Show, Eq)

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldMap f (Cmps c) = foldMap (foldMap f) c

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (\x y -> (:) <$> f x <*> y) $ pure []

-- data Triple a = Tr a a a
--   deriving (Eq, Show)

-- instance Functor Triple where
--   fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Traversable Triple where
  traverse f (Tr a b c) = Tr <$> f a <*> f b <*> f c

data Result a
  = Ok a
  | Error String
  deriving (Eq, Show)

instance Foldable Result where
  foldr _ ini (Error _) = ini
  foldr f ini (Ok result) = f result ini

instance Functor Result where
  fmap _ (Error err) = Error err
  fmap f (Ok result) = Ok $ f result

instance Traversable Result where
  traverse _ (Error err) = pure $ Error err
  traverse f (Ok result) = Ok <$> f result

-- data Tree a
--   = Nil
--   | Branch (Tree a) a (Tree a)
--   deriving (Eq, Show)

-- instance Traversable Tree where
--   traverse _ Nil = pure Nil
--   traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r

-- infixr 9 |.|
-- newtype (|.|) f g a = Cmps
--   { getCmps :: f (g a)
--   } deriving (Show, Eq)

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
  traverse f (Cmps c) = Cmps <$> traverse (traverse f) c

-- (Applicative f, Applicative g, Traversable t) => t (f (g a)) -> Compose f g (t a)
-- sequenceA . fmap Compose == Compose . fmap sequenceA . sequenceA

-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap sequenceA :: f (t (g a)) -> f (g (t a))

data OddC a
  = Un a
  | Bi a a (OddC a)
  deriving (Eq, Show)

instance Functor OddC where
  fmap f (Un a) = Un $ f a
  fmap f (Bi a b c) = Bi (f a) (f b) $ f <$> c

instance Foldable OddC where
  foldr f ini (Un a) = f a ini
  foldr f ini (Bi a b c) = f a $ f b $ foldr f ini c

instance Traversable OddC where
  traverse f (Un a) = Un <$> f a
  traverse f (Bi a b c) = Bi <$> f a <*> f b <*> traverse f c

newtype Temperature a = Temperature Double
  deriving (Num, Fractional, Eq, Show)

data Celsius
data Fahrenheit
data Kelvin

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature c) = Temperature $ c - 273.15

-- data Tree a
--   = Nil
--   | Branch (Tree a) a (Tree a)
--   deriving (Eq, Show)

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  sequenceA Nil = pure Nil
  sequenceA (Branch l x r) =
    flip <$> (Branch <$> sequenceA l) <*> sequenceA r <*> x

-- newtype PrsE a = PrsE
--   { runPrsE :: String -> Either String (a, String)
--   }

instance Monad PrsE where
  (PrsE m) >>= f = PrsE $ \s ->
    case m s of
      Left err -> Left err
      Right (v, s') ->
        case (runPrsE $ f v) s' of
          Left err -> Left err
          Right (v', s'') -> Right (v', s'')

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) c = Bi a b c
concat3OC (Un a) (Bi b1 b2 b3) c = Bi a b1 $ concat3OC (Un b2) b3 c
concat3OC (Bi a1 a2 a3) b c = Bi a1 a2 $ concat3OC a3 b c

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a) = a
concatOC (Bi a1 a2 a3) = concat3OC a1 a2 $ concatOC a3

instance Applicative OddC where
  pure = Un
  (<*>) = ap

instance Monad OddC where
  (Un a1) >>= f = f a1
  (Bi a1 a2 a3) >>= f = concat3OC (f a1) (f a2) (a3 >>= f)

-- (u <|> v) <*> w       =    u <*> w <|> v <*> w
-- 1. We know, that for (<*>) if there are any Nothing, the result will be Nothing.
-- 2. We know, that for (<|>) the result be first not Nothing if exists, or Nothing.
-- 3. if w is Nothing, then result will be Nothing.
-- 4. if u is Just, then left part will be (u <*> w) and right part will be same.
-- 5. if u is Nothing, then left part will be (v <*> w) and right part will be same.
-- 6. Hence, (u <|> v) <*> w == u <*> w <|> v <*> w

-- u1, u2 :: Maybe (Int -> Int)
-- u1 = Just $ \a -> a ^ 2
-- u2 = Nothing
-- v1, v2 :: Maybe (Int -> Int)
-- v1 = Just $ \a -> a ^ 3
-- v2 = Nothing
-- k1, k2 :: Maybe Int
-- k1 = Just 3
-- k2 = Nothing
-- test1 = ((u1 <|> v1) <*> k1) == ((u1 <*> k1) <|> (v1 <*> k1))
-- test2 = ((u1 <|> v1) <*> k2) == ((u1 <*> k2) <|> (v1 <*> k2))
-- test3 = ((u1 <|> v2) <*> k1) == ((u1 <*> k1) <|> (v2 <*> k1))
-- test4 = ((u1 <|> v2) <*> k2) == ((u1 <*> k2) <|> (v2 <*> k2))
-- test5 = ((u2 <|> v1) <*> k1) == ((u2 <*> k1) <|> (v1 <*> k1))
-- test6 = ((u2 <|> v1) <*> k2) == ((u2 <*> k2) <|> (v1 <*> k2))
-- test7 = ((u2 <|> v2) <*> k1) == ((u2 <*> k1) <|> (v2 <*> k1))
-- test8 = ((u2 <|> v2) <*> k2) == ((u2 <*> k2) <|> (v2 <*> k2))

-- (u `mplus` v) >>= k   =    (u >>= k) `mplus` (v >>= k)
-- u1, u2 :: Maybe Int
-- u1 = Just 2
-- u2 = Nothing
-- v1, v2 :: Maybe Int
-- v1 = Just 3
-- v2 = Nothing
-- k1, k2 :: Int -> Maybe Int
-- k1 a = Just $ a ^ 2
-- k2 _ = Nothing
-- test1 = ((u1 `mplus` v1) >>= k1) == ((u1 >>= k1) `mplus` (v1 >>= k1))
-- test2 = ((u1 `mplus` v1) >>= k2) == ((u1 >>= k2) `mplus` (v1 >>= k2))
-- test3 = ((u1 `mplus` v2) >>= k1) == ((u1 >>= k1) `mplus` (v2 >>= k1))
-- test4 = ((u1 `mplus` v2) >>= k2) == ((u1 >>= k2) `mplus` (v2 >>= k2))
-- test5 = ((u2 `mplus` v1) >>= k1) == ((u2 >>= k1) `mplus` (v1 >>= k1))
-- test6 = ((u2 `mplus` v1) >>= k2) == ((u2 >>= k2) `mplus` (v1 >>= k2))
-- test7 = ((u2 `mplus` v2) >>= k1) == ((u2 >>= k1) `mplus` (v2 >>= k1))
-- test8 = ((u2 `mplus` v2) >>= k2) == ((u2 >>= k2) `mplus` (v2 >>= k2))

newtype PrsEP a = PrsEP
  { runPrsEP :: Int -> String -> (Int, Either String (a, String))
  }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP p = PrsEP $ \pos ->
  let nextPos = pos + 1
  in \case
  "" -> (nextPos, Left $ "pos " ++ show nextPos ++ ": unexpected end of input")
  (x:xs) ->
    if p x
    then (nextPos, Right (x, xs))
    else (nextPos, Left $ "pos " ++ show nextPos ++ ": unexpected " ++ [x])

charEP :: Char -> PrsEP Char
charEP c = satisfyEP (== c)

instance Functor PrsEP where
  fmap f (PrsEP p) = PrsEP $ fmap (\(pos, res) ->
    case res of
      Left err -> (pos, Left err)
      Right (x, xs) -> (pos, Right (f x, xs))
    ) . p

instance Applicative PrsEP where
  pure a = PrsEP $ \pos s -> (pos, Right (a, s))
  PrsEP f <*> PrsEP p = PrsEP $ \pos s ->
    case f pos s of
      (pos', Left err) -> (pos', Left err)
      (pos', Right (g, s')) -> case p pos' s' of
        (newPos, Left err) -> (newPos, Left err)
        (newPos, Right (x, xs)) -> (newPos, Right (g x, xs))

instance Alternative PrsEP where
  empty = PrsEP $ \pos s -> (pos, Left $ "pos " ++ show pos ++ ": empty alternative")
  PrsEP f <|> PrsEP p = PrsEP $ \pos s ->
    case f pos s of
      lr@(_, Right _) -> lr
      ll@(leftPos, Left _) -> case p pos s of
        rr@(_, Right _) -> rr
        rl@(rightPos, Left _) -> if leftPos >= rightPos then ll else rl
