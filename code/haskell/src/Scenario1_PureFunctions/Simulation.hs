{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario1_PureFunctions.Simulation where

import "aeson" Data.Aeson (Value (Number))
import "aeson" Data.Aeson.KeyMap (fromList)
import "data-default" Data.Default (Default, def)
import Scenario1_PureFunctions.Model
import Simulation
import Types


-- A few orphan instances; a hint at the issues with our model.

instance Default Plants where
  def = Plant

instance Default Beans where
  def = Bean

instance Default Milk where
  def = Oat


-- | Every time the world spins, the following events occur:
--    - Businesses produce outputs.
--    - The amount of resources reduces by a number proportional to the
--      business outputs.
--    - The amount of businesses stays the same.
spin :: World -> World
spin World{resources, businesses, outputs} =
  World
    { resources  = resources - resourceCosts
    , outputs    = newOutputs ++ outputs
    , businesses = businesses
    }
  where
    newOutputs    = map output businesses
    resourceCosts = length newOutputs

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
scenario1 = simulate "Two businesses" initialWorld 51 20


-- Scenario 1a - An extra business!

gardenCenter :: (Plants, (Beans, Milk)) -> (Flowers, Coffee)
gardenCenter (p, bm) = (florist p, cafe bm)


-- Note: Our naive cost model goes wrong here; this business produces one
-- output, but actually calls the other businesses twice; so in reality it
-- produces two business outputs. But, annoyingly for us, our way of
-- representing outputs _also_ goes wrong, and these two errors cancel out.
-- In any case, it is an indication that our basic model is wrong.

scenario1a :: IO ()
scenario1a = simulate "Original + Garden center" w 51 20
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
