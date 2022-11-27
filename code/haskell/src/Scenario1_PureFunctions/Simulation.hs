{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario1_PureFunctions.Simulation where

import "aeson" Data.Aeson (Value (Number))
import "aeson" Data.Aeson.KeyMap (fromList)
import "data-default" Data.Default (Default, def)
import "base" Data.Monoid (Sum (..))
import Scenario1_PureFunctions.Model
import Simulation
import Types


-- A few orphan instances; a hint at the issues with our model.
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
--    - The amount of resources reduces by a cost of operating the businesses.
--    - The amount of businesses stays the same.
spin :: World -> World
spin World{resources, businesses, outputs} =
  World
    { resources  = resources - getSum resourceCosts
    , outputs    = newOutputs ++ outputs
    , businesses = businesses
    }
  where
    newOutputs    = map output businesses
    resourceCosts = foldMap cost' businesses

    cost' :: SomeBusiness -> Sum Int
    cost' (SomeBusiness _ (_f :: a -> b)) = cost @(a -> b)

    output :: SomeBusiness -> BusinessOutput
    output (SomeBusiness _ (f :: a -> b)) = BusinessOutput $ f (def @a)


-- We can now define an initial world state, and simulate it.

initialWorld :: World
initialWorld = World startingResources businesses []
  where
    businesses
      = [ SomeBusiness "flora's flowers" florist
        , SomeBusiness "haskell's cafe"  cafe
        ]


-- Scenario 1 - Two businesses

scenario1 :: IO ()
scenario1 = simulate "1. Two businesses" initialWorld 51 20


-- Scenario 1a - An extra business!

gardenCenter :: (Plants, (Beans, Milk)) -> (Flowers, Coffee)
gardenCenter (p, bm) = (florist p, cafe bm)

instance Cost ((Plants, (Beans, Milk)) -> (Flowers, Coffee)) where
  cost = cost @(Plants -> Flowers)
       + cost @((Beans, Milk) -> Coffee)


scenario1a :: IO ()
scenario1a = simulate "1a. + Garden center" w 51 20
  where
    i = SomeBusiness "ivory's garden center" gardenCenter
    w = initialWorld
          { businesses = i : businesses initialWorld
          }






-- Busywork for rendering

reset :: IO ()
reset = simulate "" initialWorld 1 1


instance SomeWorld World where
  spinWorld = spin
  someResources = resources
  otherPropertes w
    = fromList [ ("Resources", Number . fromIntegral . resources $ w)
               , ("Business Outputs", Number . fromIntegral . length . outputs $ w)
               ]
