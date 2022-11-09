{-# language GADTs               #-}
{-# language FlexibleInstances   #-}
{-# language NamedFieldPuns      #-}
{-# language FlexibleContexts    #-}
{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}

module Scenario1 where

import "data-default" Data.Default (Default, def)
import "base" Data.Monoid (Sum (..))

data World = World
  { resources  :: Sum Int
  , businesses :: [SomeBusiness]
  }


data SomeBusiness = forall a b. (Default a, ResourceCost (a -> b)) => SomeBusiness (a -> b)


class ResourceCost a where
  cost :: Sum Int


instance ResourceCost (Plants -> Flowers) where
  cost = Sum 1


instance Default Plants where
  def = Plant


data BusinessInput = forall a. BusinessInput a


data BusinessOutput = forall b. BusinessOutput b


businessCost :: SomeBusiness -> Sum Int
businessCost (SomeBusiness (_f :: a -> b)) = cost @(a -> b)


output :: SomeBusiness -> BusinessOutput
output (SomeBusiness (f :: a -> b)) = BusinessOutput $ f (def @a)


spin :: World -> World
spin World{resources, businesses} =
  World { resources = resources - resourceCosts , businesses }
  where
    resourceCosts = foldMap businessCost businesses
    outputs       = map output businesses







-- simulate :: WorldConfiguration -> IO ()
-- simulate = do
--   withMqtt $ \mc -> do
--     forM_ steps $ \step ->
--       send mc (toMqtt step)


-- f :: a -> b
-- ????
--  - It's a business.



data Plants  = Plant
data Flowers = Flower

florist :: Plants -> Flowers
florist = undefined



data Beans
data Milk   = Oat
data Coffee = Latte | Cappuccino

cafe :: (Beans, Milk) -> Coffee
cafe = undefined
