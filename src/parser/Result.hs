{-# LANGUAGE InstanceSigs #-}

module Result where

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data Result a
  = Success a
  | Failure String

instance Show a => Show (Result a) where
  show (Failure message) = message
  show (Success matched) = show matched

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Failure s) = Failure s
  fmap f (Success s) = Success $ f s

instance Applicative Result where
  pure :: a -> Result a
  pure = Success
  (<*>) :: Result (a -> b) -> Result a -> Result b
  (Failure f) <*> _ = Failure f
  (Success f) <*> a = f <$> a

instance Monad Result where
  return :: a -> Result a
  return = pure
  (>>=) :: Result a -> (a -> Result b) -> Result b
  (Failure s) >>= f = Failure s
  (Success s) >>= f = f s

from_result :: a -> Result a -> a
from_result _ (Success result) = result
from_result default' (Failure _) = default'

unwrap :: Result String -> String
unwrap (Success a) = a
unwrap (Failure a) = a

to_success (Success a) = a
to_success (Failure a) = error $ show a
