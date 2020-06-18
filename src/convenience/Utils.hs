module Utils where

import Data.Maybe (fromMaybe)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) a f = f <$> a

subtract_by extractor as bs =
  filter (\a -> extractor a `notElem` (extractor <$> bs)) as

(??) :: Maybe a -> a -> a
ma ?? d = fromMaybe d ma

safe_head [] = Nothing
safe_head xs = Just $ head xs

