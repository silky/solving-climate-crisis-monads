{-# language AllowAmbiguousTypes #-}

module Scenario2.Model where

import "base" Data.Monoid (Sum (..))
import Scenario2.Types

safeToConsume :: Sum Integer -> World -> Bool
safeToConsume cost' World{resources} = resources - cost' > 70

safelyProduce
  :: forall p v
   . Cost (p -> WorldState v)
  => v
  -> WorldState v
safelyProduce v = WorldState $ \w ->
  if safeToConsume (cost @(p -> WorldState v)) w
     then (Just v,  w)
     else (Nothing, w)

data Plants  = Plant
data Flowers = Flower

florist :: Plants -> WorldState Flowers
florist = const $ safelyProduce @Plants @_ Flower

instance Cost (Plants -> WorldState Flowers) where
  cost = Sum 1

data Beans  = Bean
data Milk   = Oat
data Coffee = Latte | Cappuccino

cafe :: (Beans, Milk) -> WorldState Coffee
cafe = const $ safelyProduce @(Beans, Milk) @_ Latte

instance Cost ((Beans, Milk) -> WorldState Coffee) where
  cost = Sum 1
