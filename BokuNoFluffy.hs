module BokuNoFluffy where

import Data.Char
import Data.Function

class Fluffy where
    furry :: (a -> b) -> f a -> f b

instance Fluffy [] where
    furry = map

instance Fluffy Maybe where
    furry f Nothing  = Nothing
    furry f (Just v) = Just (f v)

instance Fluffy ((->) t) where
    furry  = (.)

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

instance Fluffy  (EitherLeft t) where
    furry f (EitherLeft (Left v))  = EitherLeft (Left (f v))
    furry f (EitherLeft (Right v)) = EitherLeft (Right v)

instance Fluffy  (EitherRight t) where
    furry f (EitherLeft (Right v)) = EitherLeft (Right (f v))
    furry f (EitherLeft (Left v))  = EitherLeft (Left v)

class Misty m where
    banana :: (a -> m b) -> m a -> m b
    unicorn :: a -> m a
  
    furry' :: (a -> b) -> m a -> m b
    furry' f = banana (unicorn . f)

instance Misty [] where
    banana f  = concat . map f 
    unicorn x = [x]

instance Misty Maybe where
    banana f (Just v) = Just (f x)
    banana f Nothing  = Nothing
    unicorn x         = Just x

instance Misty ((->) t) where
    banana f g = \s -> f (g s) s
    unicorn x = \_ -> x

instance Misty (EitherLeft t) where
    banana f (EitherLeft (Left x))  = f x
    banana f (EitherLeft (Right x)) = EitherLeft (Right x)
    unicorn x                       = EitherLeft (Left x)

instance Misty (EitherRight t) where
    banana f (EitherLeft (Right x)) = f x
    banana f (EitherLeft (Left x))  = EitherLeft (Left x)
    unicorn x                       = EitherLeft (Right x)

jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

apple :: (Misty m) => m a -> m (a -> b) -> m b
apple x f = banana (\v -> banana (\g -> unicorn (g v)) f) x 

moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy xs f = 

-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = error "todo"

-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"

-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"

newtype State s a = State {
  state :: (s -> (s, a))
}

instance Fluffy (State s) where
  furry = error "todo"