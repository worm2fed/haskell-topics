module Typeclassopedia where


-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

--     (<$) :: a        -> f b -> f a
--     (<$) = fmap . const

-- instance Functor [] where
--     fmap _ []     = []
--     fmap g (x:xs) = g x : fmap g xs
  -- or we could just say fmap = map

-- instance Functor Maybe where
--     fmap _ Nothing  = Nothing
--     fmap g (Just a) = Just (g a)


-- Implement Functor instances for Either e and ((->) e)
-- instance Functor (Either e) where
--     _ `fmap` Left e  = Left e
--     g `fmap` Right a = Right $ g a

-- instance Functor ((->) e) where
--     g `fmap` r = g . r


-- Implement Functor instances for ((,) e) and for Pair
data Pair a = Pair a a

-- instance Functor ((,) e) where
--     g `fmap` (e, a) = (e, g a)

instance Functor Pair where
    g `fmap` (Pair e a) = Pair (g e) (g a)


-- Explain their similarities and differences
-- In first case the first element is unchanged, but in
-- the second it is


-- Implement a Functor instance for the type ITree
data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
    -- fmap :: (a -> b) -> ITree a -> ITree b
    g `fmap` (Leaf h)  = Leaf $ g . h
    g `fmap` (Node hs) = Node $ map (fmap g) hs


-- Give an example of a type of kind * -> * which cannot
-- be made an instance of Functor (without using undefined)
newtype K a = K (a -> Int)


-- Is this statement true or false?
-- `The composition of two Functors is also a Functor`
-- If false, give a counterexample; if true, prove it by
-- exhibiting some appropriate Haskell code

-- true, but how to show ??


-- Although it is not possible for a Functor instance to
-- satisfy the first Functor law but not the second
-- (excluding undefined), the reverse is possible
-- Give an example of a (bogus) Functor instance which
-- satisfies the second law but not the first
data Test a = Test Int a
    deriving Show

instance Functor Test where
    g `fmap` (Test c x) = Test 0 $ g x


-- instance Functor [] where
--     fmap :: (a -> b) -> [a] -> [b]
--     fmap _ [] = []
--     fmap g (x:xs) = g x : g x : fmap g xs
-- Which laws are violated by the evil Functor instance
-- for list shown above: both laws, or the first law
-- alone? Give specific counterexamples

v = [1..5]
-- 1) fmap id v -> [1,1,2,2,3,3,4,4,5,5]
--    id v -> [1,2,3,4,5]
f1 = (*2)
f2 = (+2)

a g h = fmap (g . h)
b g h = (fmap g) . (fmap h)

-- 2) a f1 f2 v -> [6,6,8,8,10,10,12,12,14,14]
--    b f1 f2 v -> [6,6,6,6,8,8,8,8,10,10,10,10,12,12,12,
--                      12,14,14,14,14]

-- both



-- class Functor f => Applicative f where
--     pure :: a -> f a

--     infixl 4 <*>, *>, <*
--     (<*>) :: f (a -> b) -> f a -> f b

--     (*>) :: f a -> f b -> f b
--     a1 *> a2 = (id <$ a1) <*> a2

--     (<*) :: f a -> f b -> f a
--     (<*) = liftA2 const


-- (Tricky) One might imagine a variant of the
-- interchange law that says something about applying
-- a pure function to an effectful argument. Using the
-- above laws, prove that
--      pure f <*> x = pure (flip ($)) <*> x <*> pure f
--      pure f <*> x = pure (flip ($)) <*> pure ($ f) <*> x
--      pure f <*> x = pure (flip ($) ($ f)) <*> x
--      pure f <*> x = pure (f $) <*> x
--      pure f <*> x = pure f <*> x


-- instance Applicative Maybe where
--     pure = Just
--     Just f <*> Just x = Just $ f <*> x

newtype ZipList a = ZipList { getZipList :: [a] }

-- instance Applicative ZipList where
--     pure x = ZipList (repeat x)
--     ZipList gs <*> ZipList xs =
            -- ZipList $ zipWith ($) gs xs

-- instance Applicative [] where
--     pure x = [x]
--     gs <*> xs = [ g x | g <- gs, b <- xs]


-- Implement a function
--      sequenceAL :: Applicative f => [f a] -> f [a]
-- There is a generalized version of this, sequenceA,
-- which works for any Traversable (see the later section
-- on Traversable), but implementing this version
-- specialized to lists is a good exercise.
sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL []     = pure []
sequenceAL (x:xs) = (:) <$> x <*> sequenceAL xs


class Functor f => Monoidal f where
    unit :: f ()
    (**) :: f a -> f b -> f (a, b)

