{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Primus.Bool
Description : boolean methods
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.Bool (
  -- * builders
  boolMaybe,
  boolEither,
  boolThese,
  boolThese',

  -- * monadic functions
  boolM,
  unlessMB,
  whenMB,
) where

import Data.Bool
import Data.These

{- | monadic version of 'Data.Bool.bool'
 predicate appears first unlike 'Data.Bool.bool'
-}
boolM :: Monad m => m Bool -> m a -> m a -> m a
boolM mb mf mt = mb >>= bool mf mt

{- | create a 'Maybe' using a predicate and a function for the success case
 predicate appears first unlike 'Data.Bool.bool'
-}
boolMaybe ::
  (a -> Bool) ->
  (a -> b) ->
  a ->
  Maybe b
boolMaybe p r a
  | p a = Just (r a)
  | otherwise = Nothing

{- | create a 'Either' using a predicate and functions for the failure and success case
 predicates appear first unlike 'Data.Bool.bool'
-}
boolEither ::
  (a -> Bool) ->
  (a -> e) ->
  (a -> b) ->
  a ->
  Either e b
boolEither p l r a
  | p a = Right (r a)
  | otherwise = Left (l a)

{- | create a 'These' using two predicates and functions for the This case and That case
   False + *     == This (a -> e)
   True  + False == That (a -> b)
   True  + True  == These (a -> e) (a -> b) -- "a" effectively appears twice

  predicates appear first unlike 'Data.Bool.bool'
-}
boolThese ::
  (a -> Bool) ->
  (a -> Bool) ->
  (a -> e) ->
  (a -> b) ->
  a ->
  These e b
boolThese p q l r = boolThese' p q l r const

{- | similar to 'boolThese' but allows you to override the 'These' case

 predicates appear first unlike 'Data.Bool.bool'
-}
boolThese' ::
  (a -> Bool) ->
  (a -> Bool) ->
  (a -> e) ->
  (a -> b) ->
  ((e, b) -> a -> (e, b)) ->
  a ->
  These e b
boolThese' p q l r b a =
  case (p a, q a) of
    (False, _) -> This (l a)
    (True, False) -> That (r a)
    (True, True) -> uncurry These (b (l a, r a) a)

-- | 'Control.Monad.unless' but makes the "a" parameter available to the callback
unlessMB ::
  Applicative m =>
  (a -> Bool) ->
  a ->
  (a -> m ()) ->
  m ()
unlessMB p a m = bool (m a) (pure ()) (p a)

-- | 'Control.Monad.when' but makes the "a" parameter available to the callback
whenMB ::
  Applicative m =>
  (a -> Bool) ->
  a ->
  (a -> m ()) ->
  m ()
whenMB p a m = bool (pure ()) (m a) (p a)
