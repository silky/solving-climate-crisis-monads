module Scenario2.Model where

import "base" Data.Monoid (Sum (..))
import "base" Data.Proxy (Proxy (..))
import Scenario2.Types

data Plants  = Plant
data Flowers = Flower

safeToConsume :: Sum Integer -> World -> Bool
safeToConsume cost' World{resources} = resources - cost' > 70

produce
  :: forall p v
   . Cost (p -> WorldState v)
  => Proxy p
  -> v
  -> WorldState v
produce _ v = WorldState $ \w ->
  if safeToConsume (cost @(p -> WorldState v)) w
     then (Just v,  w)
     else (Nothing, w)

florist :: Plants -> WorldState Flowers
florist = const $ produce (undefined :: Proxy Plants) Flower

instance Cost (Plants -> WorldState Flowers) where
  cost = Sum 1

-- instance Cost ((Beans, Milk) -> WorldState Coffee) where
--   cost = Sum 1

-- instance Default Plants where
--   def = Plant


-- data Beans  = Bean
-- data Milk   = Oat
-- data Coffee = Latte | Cappuccino

-- cafe :: (Beans, Milk) -> WorldState Coffee
-- cafe = const $ produce Latte
