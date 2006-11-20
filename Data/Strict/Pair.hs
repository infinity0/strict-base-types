-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Pair
-- Copyright   :  (c) Roman Leshchinskiy 2006
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  rl@cse.unsw.edu.au
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict pairs.
--
-- Same as regular Haskell pairs, but @(x :*: _|_) = (_|_ :*: y) = _|_@
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Strict.Pair (
    Pair(..)
#ifndef __HADDOCK__
#ifdef __GLASGOW_HASKELL__
  , (:*:)
#endif
#endif
  , fst
  , snd
  , curry
  , uncurry
) where

import Prelude hiding( fst, snd, curry, uncurry )

infixl 2 :*:

-- | The type of strict pairs.
data Pair a b = !a :*: !b deriving(Eq, Ord, Show, Read, Bounded)

#ifndef __HADDOCK__
#ifdef __GLASGOW_HASKELL__
-- This gives a nicer syntax for the type but only works on GHC for now.
type (:*:) = Pair
#endif
#endif

-- | Extract the first component of a strict pair.
fst :: Pair a b -> a
fst (x :*: _) = x

-- | Extract the second component of a strict pair.
snd :: Pair a b -> b
snd (_ :*: y) = y

-- | Curry a function on strict pairs.
curry :: (Pair a b -> c) -> a -> b -> c
curry f x y = f (x :*: y)

-- | Convert a curried function to a function on strict pairs.
uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (x :*: y) = f x y

