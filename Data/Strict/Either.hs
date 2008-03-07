-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Either
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict @Either@.
--
-- Same as the standard Haskell @Either@, but @Left _|_ = Right _|_ = _|_@
--
-----------------------------------------------------------------------------

module Data.Strict.Either (
    Either(..)
  , either
  , isLeft, isRight
  , fromLeft, fromRight
) where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Prelude hiding( Either(..), either )

-- | The strict choice type.
data Either a b = Left !a | Right !b deriving(Eq, Ord, Read, Show)

instance Functor (Either a) where
  fmap _ (Left  x) = Left x
  fmap f (Right y) = Right (f y)

instance Foldable (Either a) where
  foldr _ y (Left _)  = y
  foldr f y (Right x) = f x y

  foldl _ y (Left _)  = y
  foldl f y (Right x) = f y x

instance Traversable (Either a) where
  traverse _ (Left x)  = pure (Left x)
  traverse f (Right x) = Right <$> f x

-- | Case analysis: if the value is @'Left' a@, apply the first function to @a@;
-- if it is @'Right' b@, apply the second function to @b@.
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left  x) = f x
either _ g (Right y) = g y

-- | Yields 'True' iff the argument is of the form @Left _@.
--
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- | Yields 'True' iff the argument is of the form @Right _@.
--
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- | Extracts the element out of a 'Left' and throws an error if the argument
-- is a 'Right'.
fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _        = error "Data.Strict.Either.fromLeft: Right"

-- | Extracts the element out of a 'Right' and throws an error if the argument
-- is a 'Left'.
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "Data.Strict.Either.fromRight: Left"



