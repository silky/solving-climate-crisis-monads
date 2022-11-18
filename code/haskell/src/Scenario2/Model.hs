{-# language AllowAmbiguousTypes #-}

module Scenario2.Model where

import "base" Data.Monoid (Sum (..))
import Scenario2.Types

-- | Given some input type `i` and some output type `o`, first decide if we
-- can safely produce the output, by checking the amount of resources we would
-- need, and then produce the output and update the world, if it was indeed
-- possible to do so.
safelyProduce
  :: forall i o
   . Cost (i -> WorldState o)
  => o
  -> WorldState o
safelyProduce o =
  let c = cost @(i -> WorldState o)
   in WorldState $ \w ->
        if safeToConsume c w
           then (Just o, w { resources = resources w - c
                           , outputs   = BusinessOutput o : outputs w
                           })
           else (Nothing, w)

-- | Our safety margin.
safeToConsume :: Sum Integer -> World -> Bool
safeToConsume c World{resources} = resources - c >= 70


-- A variation on our regular business model; we now return WorldState's and
-- only produce things if we can do so safely.

data Plants  = Plant
data Flowers = Flower

florist :: Plants -> WorldState Flowers
florist = const $ safelyProduce @Plants Flower

instance Cost (Plants -> WorldState Flowers) where
  cost = Sum 1

data Beans  = Bean
data Milk   = Oat
data Coffee = Latte | Cappuccino

cafe :: (Beans, Milk) -> WorldState Coffee
cafe = const $ safelyProduce @(Beans, Milk) Latte

instance Cost ((Beans, Milk) -> WorldState Coffee) where
  cost = Sum 1
