{-# LANGUAGE FlexibleInstances #-}

module Stepick1 where

import           Data.Char
import           Data.Function
import qualified Data.List                     as L
import           Data.List.Split
import           Data.Time.Clock
import           Data.Time.Format
import           System.Locale
import           System.Directory
import           Data.Fixed

import           Prelude                 hiding ( lookup )

import           Control.Monad
import           Data.Monoid


twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then read [x, y] else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (**) (fst p2 - fst p1) 2 + (**) (snd p2 - snd p1) 2

fibonacci :: Integer -> Integer
fibonacci n | n < 0     = (^) (-1) (abs n + 1) * helper 0 1 (abs n)
            | otherwise = helper 0 1 n
 where
  helper prev _    0 = prev
  helper prev next m = helper next (next + prev) (m - 1)

seqA :: Integer -> Integer
seqA n =
  let helper _   _   _   0 = 1
      helper _   _   _   1 = 2
      helper km1 _   _   2 = km1
      helper km1 km2 km3 k = helper (km1 + km2 - 2 * km3) km1 km2 (k - 1)
  in  helper 3 2 1 n

sumNcount :: Integer -> (Integer, Integer)
sumNcount x | x /= 0    = (sum $ helper x, toInteger $ length $ helper x)
            | otherwise = (0, 1)
 where
  helper n | n /= 0    = rem (abs n) 10 : helper (div (abs n) 10)
           | otherwise = []

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ((f a + f b) / 2 + sum fx)
 where
  h  = (b - a) / 1000

  fx = helper (a + h) []

  helper n list | length list < 999 = helper (n + h) (f n : list)
                | otherwise         = list

-- multSecond ('A',2) ('E',7) -> 14
multSecond :: Num c => (a, c) -> (a, c) -> c
multSecond = (*) `on` snd

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- on op f x y = f x `op` f y
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

-- toString True => "true"
-- toString False => "false"
-- toString () => "unit type"
-- toString (False,()) => (false,unit type)"
-- toString (True,False) => "(true,false)"
class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True  = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (x, y) = "(" ++ toString x ++ "," ++ toString y ++ ")"

class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab x
    | doesEnrageMork x && doesEnrageGork x = stomp (stab x)
    | doesEnrageMork x = stomp x
    | doesEnrageGork x = stab x
    | otherwise = x

class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a
    | maxBound == a = minBound
    | otherwise = succ a

  spred :: a -> a
  spred a
    | minBound == a = maxBound
    | otherwise = pred a

instance SafeEnum Bool

avg' :: Int -> Int -> Int -> Double
avg' a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / 3

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b list = a : b : list

nTimes :: a -> Int -> [a]

-- nTimes = flip replicate
nTimes el = helper []
 where
  helper list 0 = list
  helper list n = helper (el : list) (n - 1)

        -- helper list n = helper ((el : ) $! list) (n - 1)
sndHead :: [(a, b)] -> b
sndHead = snd . head

-- sndHead ((_,y):_) = y
-- sndHead ((:) ((,) _ y) _) = y
oddsOnly :: Integral a => [a] -> [a]
oddsOnly []       = []
oddsOnly [x     ] = [ x | odd x ]
oddsOnly (x : xs) = if odd x then x : oddsOnly xs else oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse list

-- sum3 [1,2,3] [4,5] [6]
-- [11,7,3]
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (a : as) []       (c : cs) = (a + c) : sum3 as [] cs
sum3 (a : as) []       []       = a : sum3 as [] []
sum3 (a : as) (b : bs) []       = (a + b) : sum3 as bs []
sum3 []       (b : bs) []       = b : sum3 [] bs []
sum3 []       (b : bs) (c : cs) = (b + c) : sum3 [] bs cs
sum3 []       []       (c : cs) = c : sum3 [] [] cs
sum3 (a : as) (b : bs) (c : cs) = (a + b + c) : sum3 as bs cs
sum3 _        _        _        = []

-- groupElems []
-- []
-- groupElems [1,2]
-- [[1],[2]]
-- groupElems [1,2,2,2,4]
-- [[1],[2,2,2],[4]]
-- groupElems [1,2,3,2,4]
-- [[1],[2],[3],[2],[4]]
groupElems :: Eq a => [a] -> [[a]]
groupElems = helper []
 where
  helper [] (s : ss)                     = helper [[s]] ss
  helper (r : rs) (s : ss) | head r == s = helper ((s : r) : rs) ss
  helper (r : rs) (s : ss)               = helper ([s] : r : rs) ss
  helper list     []                     = reverse list

-- readDigits "365ads"
-- ("365","ads")
-- readDigits "365"
-- ("365","")
readDigits :: String -> (String, String)
readDigits = span isDigit

-- filterDisj (< 10) odd [7,8,10,11,12]
-- [7,8,11]
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x : xs) | p1 x || p2 x = x : filterDisj p1 p2 xs
                          | otherwise    = filterDisj p1 p2 xs

-- qsort [1,3,2,5]
-- [1,2,3,5]
qsort :: Ord a => [a] -> [a]
qsort []       = []
qsort (x : xs) = qsort (filter (< x) xs) ++ x : qsort (filter (>= x) xs)

-- squaresNcubes [3,4,5]
-- [9,27,16,64,25,125]
-- squaresNcubes :: Num a => [a] -> [a]
-- squaresNcubes []     = []
-- squaresNcubes (x:xs) = (x ^ 2) : (x ^ 3) : squaresNcubes xs
-- perms [1,2,3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
perms :: [a] -> [[a]]
perms = foldr (concatMap . insert2list) [[]]

-- insert element to every position in list
insert2list :: a -> [a] -> [[a]]
insert2list e = helper [] []
 where
  helper result history [] = (history ++ [e]) : result
  helper result history (x : xs) =
    helper ((history ++ [e] ++ [x] ++ xs) : result) (history ++ [x]) xs

-- delAllUpper "Abc IS not ABC"
-- "Abc not"
delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

-- max3 [7,2,9] [3,6,8] [1,8,10]
-- [7,8,10]
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> max x (max y z))

-- take 10 $ fibStream
-- [0,1,1,2,3,5,8,13,21,34]
fibStream :: [Integer]
fibStream = zipWith (+) (0 : fibStream) (0 : 1 : fibStream)

-- repeat x = xs where xs = x : xs
repeat :: a -> [a]
repeat = iterate repeatHelper where repeatHelper = id

-- data Odd = Odd Integer
--   deriving (Eq, Show)
-- instance Enum Odd where
--     toEnum x         = Odd(toInteger x)
--     fromEnum (Odd x) = fromInteger x
--     succ (Odd x)     = Odd(x + 2)
--     pred (Odd x)     = Odd(subtract 2 x)
--     enumFrom (Odd x)                         = map Odd [x, x + 2 ..]
--     enumFromThen (Odd x) (Odd y)             = map Odd [x, y ..]
--     enumFromTo (Odd x) (Odd y)               = map Odd [x, x + 2 .. y]
--     enumFromThenTo (Odd x1) (Odd x2) (Odd y) = map Odd [x1, x2 .. y]
coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

-- change 7
-- [[2,2,3],[2,3,2],[3,2,2],[7]]
change :: (Ord a, Num a) => a -> [[a]]
change 0 = []
change amount =
  [ coin : result
  | coin <- coins
  , amount >= coin
  , result <- change (amount - coin)
  ]

-- foldr (-) 0 [1,2,3,4]
-- (1 - (2 - (3 - (4 - 0)))) = -2
--   -
--  / \
-- 1   -
--    / \
--   2   -
--      / \
--     3   -
--        / \
--       4   0
-- foldl (-) 0 [1,2,3,4]
-- ((((0 - 1) - 2) - 3) - 4) = -10
--         -
--        / \
--       -   4
--      / \
--     -   3
--    / \
--   -   2
--  / \
-- 0   1
-- concatList [[1,2],[],[3]]
-- [1,2,3]
-- concatList :: [[a]] -> [a]
-- concatList = foldr (++) []
-- lengthList [7,6,5]
-- 3
lengthList :: [a] -> Int
lengthList = foldr (\_ xs -> 1 + xs) 0

-- sumOdd [2,5,30,37]
-- 42
sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

-- meanList [1,2,3,4]
-- 2.5
meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (s, l) -> (x + s, 1 + l)) (0, 0)

-- evenOnly [1..10]
-- [2,4,6,8,10]
-- evenOnly ['a'..'z']
-- "bdfhjlnprtvxz"
evenOnly :: [a] -> [a]
evenOnly = snd . foldr (\a ~(xs, ys) -> (a : ys, xs)) ([], [])

lastElem :: [a] -> a
lastElem = foldl1 (\_ b -> b)

-- revRange ('a','z')
-- "zyxwvutsrqponmlkjihgfedcba"
revRange :: (Char, Char) -> String
revRange (a, b) = reverse $ L.unfoldr g (a, b)
 where
  g (ini, _) | ini >= a && ini <= b = Just (ini, (succ ini, b))
             | otherwise            = Nothing

data Color = Red
           | Green
           | Blue

instance Show Color where
  show a = case a of
    Red   -> "Red"
    Green -> "Green"
    Blue  -> "Blue"

data LogLevel = Error
              | Warning
              | Info

instance Enum LogLevel where
  fromEnum Error   = 3
  fromEnum Warning = 2
  fromEnum Info    = 1

  toEnum a = case a of
    3 -> Error
    2 -> Warning
    1 -> Info
    _ -> undefined

-- cmp Error Warning
-- GT
-- cmp Info Warning
-- LT
-- cmp Warning Warning
-- EQ
cmp :: LogLevel -> LogLevel -> Ordering
cmp a b = compare (fromEnum a) (fromEnum b)

data Result = Fail
            | Success

-- doSomeWork :: SomeData -> (Result,Int)
-- processData :: SomeData -> String
-- processData a = case doSomeWork a of
--     (Success, _) -> "Success"
--     (Fail, code) -> "Fail: " ++ show code
data Point' = Point' Double Double

origin :: Point'
origin = Point' 0.0 0.0

distanceToOrigin :: Point' -> Double
distanceToOrigin (Point' x y) = sqrt (x ** 2 + y ** 2)

distance1 :: Point' -> Point' -> Double
distance1 (Point' x1 y1) (Point' x2 y2) =
  sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

data Result' = Fail' Int
             | Success'

instance Show Result' where
  show a = case a of
    (Fail' code) -> "Fail: " ++ show code
    Success'     -> "Success"

data Shape = Circle Double
           | Rectangle Double Double
  deriving Show

area :: Shape -> Double
area figure = case figure of
  (Circle r     ) -> pi * r ** 2
  (Rectangle a b) -> a * b

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare shape = case shape of
  (Rectangle a b) -> a == b
  _               -> False

data Bit = O
         | I
  deriving Show

data Sign = Minus'
          | Plus'
  deriving Show

data Z = Z Sign [Bit]
  deriving Show

b2d :: Z -> Integer
b2d (Z s bits) = sign * fst (foldl f (0, 0) bits)
 where
  sign = case s of
    Plus'  -> 1
    Minus' -> -1

  f (res, pos) I = (2 ^ pos + res, pos + 1)
  f (res, pos) O = (res, pos + 1)

d2b :: Integer -> Z
d2b a = Z sign $ helper (abs a) []
 where
  sign = if a < 0 then Minus' else Plus'

  helper 0 l = reverse l
  helper n l = helper (div n 2) (getBit (mod n 2) : l)

  getBit b = if b == 1 then I else O

addZ :: Z -> Z -> Z
addZ a b = d2b $ b2d a + b2d b

mulZ :: Z -> Z -> Z
mulZ a b = d2b $ b2d a * b2d b

timeToString :: UTCTime -> String
timeToString = formatTime Data.Time.Format.defaultTimeLocale "%a %d %T"

data LogEntry =
  LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString a = case a of
  Error   -> "Error"
  Warning -> "Warning"
  Info    -> "Info"

logEntryToString :: LogEntry -> String
logEntryToString a =
  timeToString (timestamp a)
    ++ ": "
    ++ logLevelToString (logLevel a)
    ++ ": "
    ++ message a

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     }
  deriving Show

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 { lastName = p1 & lastName }

abbrFirstName :: Person -> Person
abbrFirstName p = if length (firstName p) < 2
  then p
  else p { firstName = head (firstName p) : ['.'] }


data Coord a = Coord a a
    deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

getCenter :: Double -> Coord Int -> Coord Double
getCenter a (Coord x y) =
  Coord (fromIntegral x * a + a / 2) (fromIntegral y * a + a / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell a (Coord x y) = Coord (div' x a) (div' y a)


findDigit :: String -> Maybe Char
findDigit []       = Nothing
findDigit (x : xs) = if isDigit x then Just x else findDigit xs

findDigitOrX :: String -> Char
findDigitOrX a = case findDigit a of
  Nothing  -> 'X'
  (Just x) -> x


maybeToList :: Maybe a -> [a]
maybeToList a = case a of
  (Just x) -> [x]
  Nothing  -> []

listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (a : _) = Just a


data Error = ParsingError | IncompleteDataError | IncorrectDataError String
  deriving Show


{-
Строка, которая подается на вход, должна разбивать по символу '\n' на список строк, каждая из которых имеет вид X = Y. Если входная строка не имеет указанный вид, то функция должна возвращать ParsingError.
Если указаны не все поля, то возвращается IncompleteDataError.
Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
Если в строке присутствуют лишние поля, то они игнорируются.
-}
-- parsePerson :: String -> Either Error Person
-- parsePerson a = checkFormat $ map (word2cort . splitOn " = ") $ lines a

writeFields :: Person -> [(String, String)] -> Either Error Person
writeFields p []              = Right p
writeFields p ((x1, x2) : xs) = case x1 of
  "firstName" -> writeFields (p { firstName = x2 }) xs
  "lastName"  -> writeFields (p { lastName = x2 }) xs
  "age"       -> if filter isNumber x2 == x2
    then writeFields (p { age = read x2 :: Int }) xs
    else Left (IncorrectDataError x2)

-- checkFormat :: [Maybe (String, String)] -> Either Error Person
-- checkFormat a
--   | null a = Left ParsingError
--   | Nothing `elem` a = Left ParsingError
--   | otherwise = if L.sort (getKeys (clean a)) /= fields
--     then Left IncompleteDataError
--     else writeFields Person{} (clean a)

fields :: [String]
fields = L.sort ["firstName", "lastName", "age"]

getKeys :: [(String, String)] -> [String]
getKeys []            = []
getKeys ((k, v) : xs) = k : getKeys xs

clean :: [Maybe (String, String)] -> [(String, String)]
clean list = foldr (\(Just (x1, x2)) res -> (x1, x2) : res)
                   []
                   (filter (\(Just (a1, a2)) -> a1 `elem` fields) list)

word2cort :: [String] -> Maybe (String, String)
word2cort [_] = Nothing
word2cort (x1 : xs) | null xs   = Nothing
                    | otherwise = Just (x1, head xs)


data List a = Nil | Cons a (List a)

fromList' :: List a -> [a]
fromList' Nil         = []
fromList' (Cons x xs) = x : fromList' xs

toList :: [a] -> List a
toList = foldr Cons Nil


data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero    = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc $ toNat $ n - 1

add :: Nat -> Nat -> Nat
add a b = toNat $ fromNat a + fromNat b

mul :: Nat -> Nat -> Nat
mul a b = toNat $ fromNat a * fromNat b

fac :: Nat -> Nat
fac a = toNat $ helper (fromNat a)
 where
  helper 0 = 1
  helper n = n * helper (n - 1)


-- data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
--   deriving Show

-- height :: Tree' a -> Int
-- height (Leaf' _  ) = 0
-- height (Node' a b) = max (1 + height a) (1 + height b)

-- size :: Tree' a -> Int
-- size (Leaf' _  ) = 1
-- size (Node' a b) = 1 + size a + size b

-- avg :: Tree' Int -> Int
-- avg t = let (c, s) = go t in s `div` c
--  where
--   go :: Tree' Int -> (Int, Int)
--   go (Leaf' n  ) = (1, n)
--   go (Node' a b) = sumTupple (go a) (go b)
--   sumTupple (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)


infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand' :: Expr -> Expr
expand' e = case e of
  ((e1 :+: e2) :*: e) -> expand' e1 :*: expand' e :+: expand' e2 :*: expand' e
  (e :*: (e1 :+: e2)) -> expand' e :*: expand' e1 :+: expand' e :*: expand' e2
  (e1 :+: e2) -> expand' e1 :+: expand' e2
  (e1 :*: e2) -> expand' e1 :*: expand' e2
  e -> e
expand e = let new = expand' e in if new == e then e else expand (expand' e)

a = (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
b = Val 1 :*: (Val 2 :+: Val 3) :*: Val 4


-- newtype Xor = Xor { getXor :: Bool }
  -- deriving (Eq,Show)

-- instance Monoid Xor where
--   mempty = Xor (False)
--   (Xor a) `mappend` (Xor b) = Xor (a `xor` b)

-- True  `xor` False = True
-- False `xor` True  = True
-- _     `xor` _     = False

-- newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
--   deriving (Eq, Show)

-- instance Monoid a => Monoid (Maybe' a) where
--   mempty = Maybe' (Just mempty)
--   Maybe' (Just a) `mappend` Maybe' (Just b) = Maybe' (Just a `mappend` Just b)
--   _               `mappend` _               = Maybe' (Nothing)


{-
Ниже приведено определение класса MapLike типов, похожих на тип Map.

Определите представителя MapLike для типа ListMap, определенного ниже как список пар ключ-значение.
Для каждого ключа должно храниться не больше одного значения.
Функция insert заменяет старое значение новым, если ключ уже содержался в структуре.
-}
class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k,v)] -> m k v
  fromList []          = empty
  fromList ((k, v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
  deriving (Eq, Show)

instance MapLike ListMap where
  empty = ListMap []
  lookup k = L.lookup k . getListMap
  insert k v = ListMap . ((k, v) :) . getListMap . delete k
  delete k = ListMap . filter ((/= k) . fst) . getListMap

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
  empty = ArrowMap (\_ -> Nothing)
  lookup k m = getArrowMap m k
  insert k v m =
    ArrowMap (\key -> if key == k then Just v else getArrowMap m key)
  delete k m =
    ArrowMap (\key -> if key == k then Nothing else getArrowMap m key)
  fromList []            = empty
  fromList ((k, v) : xs) = insert k v (fromList xs)


data Point3D a = Point3D a a a
  deriving Show

instance Functor Point3D where
  fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)


data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
  fmap f (Point a        ) = Point (fmap f a)
  fmap f (LineSegment a b) = LineSegment (fmap f a) (fmap f b)


-- data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a)
--   deriving Show

-- instance Functor Tree where
--   fmap f (Leaf a      ) = Leaf (f <$> a)
--   fmap f (Branch l v r) = Branch (f <$> l) (f <$> v) (f <$> r)


data Entry k1 k2 v = Entry (k1, k2) v
  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]
  deriving Show

instance Functor (Entry k v) where
  fmap f (Entry k v) = Entry k (f v)

instance Functor (Map k v) where
  fmap f (Map list) = Map $ map (f <$>) list


data Log a = Log [String] a
  deriving Show
{-
add1Log = toLogger (+1) "added one"
add1Log 3 => Log ["added one"] 4
-}
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg v = Log [msg] $ f v
{-
execLoggers 3 add1Log mult2Log => Log ["added one","multiplied by 2"] 8
-}
execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers v l1 l2 =
  (\(Log msg1 a) -> (\(Log msg2 c) -> Log (msg1 ++ msg2) c) $ l2 a) $ l1 v

returnLog :: a -> Log a
returnLog = Log []
{-
Log ["nothing done yet"] 0 `bindLog` add1Log => Log ["nothing done yet","added one"] 1
 -}
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg v) f = (\(Log m a) -> Log (msg ++ m) a) $ f v

add1Log = toLogger (+ 1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

instance Functor Log where
  fmap = liftM
instance Applicative Log where
  pure  = return
  (<*>) = ap

instance Monad Log where
  return = returnLog
  (>>=)  = bindLog
{-
execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)] => Log ["added one","multiplied by 2","multiplied by 100"] 800
-}
execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a []       = return a
execLoggersList a (f : fs) = f a >>= (`execLoggersList` fs)


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
  deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken a   = if foldr (\v res -> res && isDigit v) True a
  then Just (Number (read a :: Int))
  else Nothing

tokenize :: String -> Maybe [Token]
tokenize = mapM asToken . words


-- data Board = Board String
-- nextPositions :: Board -> [Board]
-- nextPositions = undefined

-- nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
-- nextPositionsN b n pred = filter pred $ helper n b
--  where
--   helper n b | n < 0     = []
--              | n == 0    = return b
--              | otherwise = nextPositions b >>= helper (n - 1)


-- [(a, b, c) | a <- [1..], b <- [1..a], c <- [1..a+b], a^2 + b^2 == c^2]
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x
  | x <= 0 = []
  | otherwise = do
    a    <- [1 .. x]
    b    <- [a .. x]
    c    <- [b .. x]
    True <- return (a ^ 2 + b ^ 2 == c ^ 2)
    return (a, b, c)


main'' :: IO ()
main'' = do
  putStrLn "What is your name?"
  putStr "Name: "
  name <- getLine
  if null name then main'' else putStrLn $ "Hi, " ++ name ++ "!"

main' :: IO ()
main' = do
  putStr "Substring: "
  sub <- getLine
  case sub of
    "" -> putStrLn "Canceled"
    _  -> do
      allFiles <- getDirectoryContents "."
      mapM_
          (\file -> do
            putStrLn $ "Removing file: " ++ file
            removeFile file
          )
        $ filter (L.isInfixOf sub) allFiles


instance Functor (Reader r)  where
  fmap = liftM
instance Applicative (Reader r)  where
  pure  = return
  (<*>) = ap
newtype Reader r a = Reader { runReader :: r -> a }

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ runReader m . f

asks :: (r -> a) -> Reader r a
asks = Reader

type User = String
type Password = String
type UsersTable = [(User, Password)]

users :: UsersTable
users = [("user", "123456"), ("x", "hi"), ("root", "123456")]
-- runReader usersWithBadPasswords users
-- ["user","root"]
usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = asks $ foldr
  (\(u, p) a -> case p of
    "123456" -> u : a
    _        -> a
  )
  []

instance (Monoid w) => Functor (Writer w)  where
  fmap = liftM
instance (Monoid w) => Applicative (Writer w)  where
  pure  = return
  (<*>) = ap
newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  m >>= k =
    let (x, u) = runWriter m
        (y, v) = runWriter $ k x
    in  Writer (y, u <> v)

tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

execWriter :: Writer w a -> w
execWriter = snd . runWriter

evalWriter :: Writer w a -> a
evalWriter = fst . runWriter

type Shopping = Writer (Sum Integer, [String]) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"   180
  purchase "Lettuce" 328

purchase :: String -> Integer -> Shopping
purchase item cost = tell (Sum cost, [item])

total :: Shopping -> Integer
total = getSum . fst . execWriter

items :: Shopping -> [String]
items = snd . execWriter


instance Functor (State s)  where
  fmap = liftM
instance Applicative (State s)  where
  pure  = return
  (<*>) = ap
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return a = State $ \st -> (a, st)

  m >>= k = State $ \st ->
    let (a, st') = runState m st
        m'       = k a
    in  runState m' st'

execState :: State s a -> s -> s
execState m = snd . runState m

evalState :: State s a -> s -> a
evalState m = fst . runState m

get :: State s s
get = State $ \st -> (st, st)

put :: s -> State s ()
put st = State $ const ((), st)

modify :: (s -> s) -> State s ()
modify f = State $ \st -> ((), f st)


readerToState :: Reader r a -> State r a
readerToState (Reader m) = State $ \st -> (m st, st)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = State $ \st -> (evalWriter m, st <> execWriter m)


fibStep :: State (Integer, Integer) ()
fibStep = do
  (a1, a2) <- get
  put (a2, a1 + a2)

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState $ replicateM_ n m

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)


data Tree a = Leaf a | Fork (Tree a) a (Tree a)
  deriving Show

-- numberTree (Leaf ())
-- Leaf 1
-- numberTree (Fork (Leaf ()) () (Leaf ()))
-- Fork (Leaf 1) 2 (Leaf 3)
-- numberTree :: Tree () -> Tree Integer
-- numberTree tree = evalState action (0, tree)
--  where
--   action :: State (Integer, Tree ()) (Tree Integer)
--   action = do
--     (n, tree) <- get
--     case tree of
--       (Leaf _           ) -> do
--         put (succ n, tree)
--         Leaf . fst <$> get

--       (Fork left _ right) -> do
--         put (succ n, left)
--         t_left <- action

--         (n, _) <- get
--         put (succ n, right)
--         t_right <- action

--         (n, _)  <- get
--         return $ Fork t_left (succ n) t_right

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (action tree) 1
 where
  action :: Tree () -> State Integer (Tree Integer)
  action (Leaf _           ) = Leaf <$> get
  action (Fork left _ right) = do
    t_left <- action left
    modify succ
    n <- get
    modify succ
    t_right <- action right
    return $ Fork t_left n t_right



chr2code :: IO ()
chr2code = do
  putStrLn "Enter a string you want to convert"
  putStr "String: "
  str <- getLine
  if null str then chr2code else putStrLn $ convert str
 where
  convert :: String -> String
  convert []       = []
  convert (x : xs) = x : show (ord x) ++ convert xs
