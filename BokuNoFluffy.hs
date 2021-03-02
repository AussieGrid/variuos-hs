module BokuNoFluffy where

import Data.Char
import Data.Function

class Fluffy where
    furry :: (a -> b) -> f a -> f b

instance Fluffy [] where
    furry = map

data Maybe a = Nothing | Just a

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
