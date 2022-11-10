{-# language FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario1.Simulation where

import "aeson" Data.Aeson (Object)
import "data-default" Data.Default (Default, def)
import "base" Data.Monoid (Sum (Sum))
import Scenario1.Model
import Scenario1.Types


-- Below we pay the price for not modelling our domain properly; we define a
-- bunch of orphan instances.

instance Cost (Plants -> Flowers) where
  cost = Sum 1

instance Cost ((Beans, Milk) -> Coffee) where
  cost = Sum 1

instance Default Plants where
  def = Plant

instance Default Beans where
  def = Bean

instance Default Milk where
  def = Oat


-- | Every time the world spins, the following events occur:
--    - Businesses produce outputs.
--    - The amount of resources reduces precisely by the cost of operating businesses.
--    - The amount of businesses stays the same.
spin :: World -> World
spin World{resources, businesses, outputs} =
  World
    { resources  = resources - resourceCosts
    , businesses = businesses
    , outputs    = newOutputs ++ outputs
    }
  where
    resourceCosts = foldMap cost' businesses
    newOutputs    = map output businesses



data EarthUpdate = EarthUpdate
  { step :: Double
  , blob :: Object
  }

-- toMqtt :: World -> Double
-- toMqtt World{resources} =
--   1 - fromInteger (getSum resources / initialResources)


initialResources :: Integer
initialResources = 100


initialWorld :: World
initialWorld = World (Sum initialResources) businesses []
  where
    businesses
      = [ SomeBusiness "flora's flowers" florist
        , SomeBusiness "haskell cafe"    cafe
        ]


-- simulate :: Int -> World -> IO ()
-- simulate n world = do
--   withMqtt $ \mc -> do
--     void $ foldM (f mc) world [1 .. n]
--   where
--     f w _ = do
--       let w' = spin w
--       send mc (toMqtt w)
--       pure w'
