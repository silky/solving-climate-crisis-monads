{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario1_PureFunctions.Simulation where

import "aeson" Data.Aeson (Value (Number))
import "aeson" Data.Aeson.KeyMap (fromList)
import "data-default" Data.Default (Default, def)
import "base" Data.Monoid (Sum (..))
import Scenario1_PureFunctions.Model
import Simulation
import Types


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
    , outputs    = newOutputs ++ outputs
    , businesses = businesses
    }
  where
    resourceCosts = foldMap cost' businesses
    newOutputs    = map output businesses

    -- Because of the way our representation works, we need to pattern-match to
    -- call our functions at the right type.

    cost' :: SomeBusiness -> Sum Integer
    cost' (SomeBusiness _ (_ :: a -> b)) = cost @(a -> b)

    output :: SomeBusiness -> BusinessOutput
    output (SomeBusiness _ (f :: a -> b)) = BusinessOutput $ f (def @a)


-- We can now define an initial world state, and simulate it.

initialWorld :: World
initialWorld = World (Sum startingResources) businesses []
  where
    businesses
      = [ SomeBusiness "flora's flowers" florist
        , SomeBusiness "haskell's cafe"  cafe
        ]


-- | Busywork so we can render it.
instance SomeWorld World where
  spinWorld = spin
  someResources = resources
  otherPropertes w
    = fromList [ ("Resources", Number $ fromInteger $ getSum $ resources w)
               , ("Business Outputs", Number $ fromInteger $ toInteger $ length $ outputs $ w)
               ]


-- Scenario 1 - Two businesses

scenario1 :: IO ()
scenario1 = simulate initialWorld 51 20





-- Scenario 1a - An extra business!

gardenCenter :: (Plants, (Beans, Milk)) -> (Flowers, Coffee)
gardenCenter (p, bm) = (florist p, cafe bm)


instance Cost ((Plants, (Beans, Milk)) -> (Flowers, Coffee)) where
  cost = cost @(Plants -> Flowers)
       + cost @((Beans, Milk) -> Coffee)


scenario1a :: IO ()
scenario1a = simulate w 51 20
  where
    gc = SomeBusiness "ivory's garden center" gardenCenter
    w = initialWorld
          { businesses = gc : businesses initialWorld
          }


reset :: IO ()
reset = simulate initialWorld 1 1

