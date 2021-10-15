{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE TypeOperators #-}

module Stepik2 where

-- import Text.Parsec (Parsec, char, digit, many1, sepBy)
import Control.Applicative (ZipList(..), (<**>), Alternative(empty, (<|>)), Applicative (liftA2))
import Control.Monad (ap, MonadPlus (mplus), liftM, forM, when)
import Control.Monad.Trans (lift, MonadTrans)
import Control.Monad.Trans.Cont (Cont, runCont, cont)
import Control.Monad.Trans.Except (Except, except, throwE, runExceptT, withExcept, runExcept, ExceptT)
import Control.Monad.Trans.Reader (Reader, asks, ReaderT (..), ask)
import Control.Monad.Trans.State (State, get, put, runState, execState, StateT (runStateT), execStateT)
import Control.Monad.Trans.Writer (WriterT (..), tell, Writer, runWriter)
import Data.Char (isDigit, toUpper)
import Data.Monoid (Any(..), All (..), Endo(..), Last (..))
import Data.Semigroup (Semigroup(..))
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

a' :: A
a' = Cmps (4, ('2', True))

b' :: B t
b' = Cmps (False, id, Right 42)

c' :: C
c'  = Cmps $ \_ _ -> 42

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


-- Monads and Effects


-- withExcept :: (e -> e') -> Except e a -> Except e' a
-- withExcept f (Except (Left err)) = Except . Left $ f err
-- withExcept _ (Except (Right res)) = Except $ Right res

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Show, Eq)

infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
xs !!! i
  | i < 0 = throwE ErrNegativeIndex
  | null $ drop i xs = throwE $ ErrIndexTooLarge i
  | otherwise = return $ xs !! i

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead s = case reads s of
  [(x, "")] -> return x
  _ -> throwE $ NoParse s

data SumError = SumError Int ReadError
  deriving Show

trySum :: [String] -> Except SumError Integer
trySum xs = sum <$> traverse traverseF (zip [1..] xs)
  where
    traverseF :: (Int, String) -> Except SumError Integer
    traverseF (i, x) = SumError i `withExcept` tryRead x

newtype SimpleError = Simple
  { getSimple :: String
  } deriving (Show, Eq)

instance Semigroup SimpleError where
  (Simple s1) <> (Simple s2) = Simple $ s1 <> s2

instance Monoid SimpleError where
  mempty = Simple ""

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"
lie2se ErrNegativeIndex = Simple "[negative index]"

newtype Validate e a = Validate
  { getValidate :: Either [e] a
  }

instance Functor (Validate e) where
  fmap f (Validate x) = Validate $ f <$> x

instance Applicative (Validate e) where
  pure = Validate . pure
  (Validate f) <*> (Validate x) = Validate $ f <*> x

collectE :: Except e a -> Validate e a
collectE e = case runExcept e of
  Right r -> Validate $ Right r
  Left l  -> Validate $ Left [l]

validateSum :: [String] -> Validate SumError Integer
validateSum xs = Validate $ foldr calculate (Right 0) $ prepare <$> zip [1..] xs
  where
    calculate (Validate x) res = case x of
      Left err -> either (\v -> Left $ err <> v) (\_ -> Left err) res
      Right x -> either Left (\v -> Right $ v + x) res

    prepare :: (Int, String) -> Validate SumError Integer
    prepare (i, x) = collectE $ SumError i `withExcept` tryRead x

decode :: (Int -> r) -> r
decode c = c 0

as, a :: Int -> (Int -> r) -> r
as n c = c n
a n c = c n

number :: a -> a
number = id

twenty, hundred, thousand, seventeen, one, two, three :: Int -> (Int -> r) -> r
twenty n c = c $ 20 + n
hundred n c = c $ 100 * n
thousand n c = c $ 1000 * n
seventeen n c = c $ n + 17
one n c = c $ n + 1
two n c = c $ n + 2
three n c = c $ n + 3

showCont :: Show a => Cont String a -> String
showCont m = runCont m show

addTens :: Int -> Checkpointed Int
addTens x1 checkpoint = do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}

type Checkpointed a = (a -> Cont a a) -> Cont a a

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed p checkpointed = runCont check id
  where
    check = checkpointed $ \v -> cont $ \c -> if p (c v) then c v else v

newtype FailCont r e a = FailCont
  { runFailCont :: (a -> r) -> (e -> r) -> r
  }

instance Functor (FailCont r e) where
  fmap = liftM

instance Applicative (FailCont r e) where
  pure x = FailCont $ \ok _ -> ok x
  (<*>) = ap

instance Monad (FailCont r e) where
  FailCont fc >>= k = FailCont $ \ok notOk ->
    fc (\ok' -> runFailCont (k ok') ok notOk) notOk

toFailCont :: Except e a -> FailCont r e a
toFailCont ex = case runExcept ex of
  Left err -> FailCont $ \_ notOk -> notOk err
  Right v -> FailCont $ \ok _ -> ok v

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont ex = runFailCont ex Right Left

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont $ \ok notOk ->
  runFailCont (f $ \a -> FailCont $ \_ _ -> ok a) ok notOk

logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
  el1 <- lift $ asks head
  el2 <- lift $ asks (map toUpper . head . tail)
  tell el1
  return el2

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 xs = do
  tell $ filter p1 xs
  lift . tell $ filter p2 xs
  return $ filter (\x -> not (p1 x) && not (p2 x)) xs

type MyRW = ReaderT [String] (Writer String)

runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW rw e = runWriter $ runReaderT rw e

logFirstAndRetSecondMyRW :: MyRW String
logFirstAndRetSecondMyRW = do
  el1 <- myAsks head
  el2 <- myAsks (map toUpper . head . tail)
  myTell el1
  return el2

myRWAsks :: ([String] -> a) -> MyRW a
myRWAsks = asks

myRWTell :: String -> MyRW ()
myRWTell = lift . tell

type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rw e = runWriterT $ runReaderT rw e

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myTell :: Monad m => String -> MyRWT m ()
myTell = lift . tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift

logFirstAndRetSecondMyRWT :: MyRWT IO String
logFirstAndRetSecondMyRWT = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2

logFirstAndRetSecondSafe :: MyRWT Maybe String
logFirstAndRetSecondSafe = do
  xs <- ask
  case xs of
    (el1 : el2 : _) -> myTell el1 >> return (map toUpper el2)
    _ -> myLift Nothing

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  xs <- ask
  let eXs = take 2 [x | x <- xs, even $ length x]
  let oXs = take 2 [x | x <- xs, odd $ length x]
  when (length eXs < 2 || length oXs < 2) $ myLift Nothing
  myTell (head eXs) >> myTell "," >> myTell (head oXs)
  return (up eXs, up oXs)
  where
    up = map toUpper . head . tail

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi computation = runState (runExceptT computation)

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go low high action = do
  v <- lift get
  lift . put $ execState action v
  current <- lift get
  when (current <= low) $ throwE "Lower bound"
  when (current >= high) $ throwE "Upper bound"

type RiiEsSiT m = ReaderT (Integer, Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT
  :: ReaderT (Integer, Integer) (ExceptT String (StateT Integer m)) a
  -> (Integer, Integer)
  -> Integer
  -> m (Either String a, Integer)
runRiiEsSiT computation e = runStateT (runExceptT $ runReaderT computation e)

go' :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go' action = do
  (low, high) <- ask
  v <- lift $ lift get
  current <- lift . lift . lift $ execStateT action v
  lift . lift . put $ current
  when (current <= low) $ lift $ throwE "Lower bound"
  when (current >= high) $ lift $ throwE "Upper bound"

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n

-- newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr2T e1 e2 m a = Arr2T
  { getArr2T :: e1 -> e2 -> m a
  }

-- newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }
newtype Arr3T e1 e2 e3 m a = Arr3T
  { getArr3T :: e1 -> e2 -> e3 -> m a
  }

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T $ \e1 -> return . f e1

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T $ \e1 e2 -> return . f e1 e2

instance Functor m => Functor (Arr2T e1 e2 m) where
  fmap :: (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
  fmap f tr = Arr2T $ \e1 -> fmap f . getArr2T tr e1

instance Functor m => Functor (Arr3T e1 e2 e3 m) where
  fmap :: (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
  fmap f tr = Arr3T $ \e1 e2 -> fmap f . getArr3T tr e1 e2

instance Applicative m => Applicative (Arr2T e1 e2 m) where
  pure :: a -> Arr2T e1 e2 m a
  pure x = Arr2T $ \_ _ -> pure x

  (<*>) :: Arr2T e1 e2 m (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
  f <*> v = Arr2T $ \e1 -> liftA2 (<*>) (getArr2T f e1) (getArr2T v e1)

instance Applicative m => Applicative (Arr3T e1 e2 e3 m) where
  pure :: a -> Arr3T e1 e2 e3 m a
  pure x = Arr3T $ \_ _ _ -> pure x

  (<*>) :: Arr3T e1 e2 e3 m (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
  f <*> v = Arr3T $ \e1 e2 -> liftA2 (<*>) (getArr3T f e1 e2) (getArr3T v e1 e2)

instance Monad m => Monad (Arr2T e1 e2 m) where
  (>>=) :: Arr2T e1 e2 m a -> (a -> Arr2T e1 e2 m b) -> Arr2T e1 e2 m b
  m >>= k = Arr2T $ \e1 e2 -> do
    v <- getArr2T m e1 e2
    getArr2T (k v) e1 e2

instance Monad m => Monad (Arr3T e1 e2 e3 m) where
  (>>=) :: Arr3T e1 e2 e3 m a -> (a -> Arr3T e1 e2 e3  m b) -> Arr3T e1 e2 e3 m b
  m >>= k = Arr3T $ \e1 e2 e3 -> do
    v <- getArr3T m e1 e2 e3
    getArr3T (k v) e1 e2 e3

instance MonadFail m => MonadFail (Arr3T e1 e2 e3 m) where
  fail :: String -> Arr3T e1 e2 e3 m a
  fail s = Arr3T $ \_ _ _ -> fail s

instance MonadTrans (Arr2T e1 e2) where
  lift :: Monad m => m a -> Arr2T e1 e2 m a
  lift m = Arr2T $ \_ _ -> m

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \e1 e2 -> return $ f e1 e2
