{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario3_EmissionsTracking.Simulation where

import "base" Control.Monad (forM)
import "aeson" Data.Aeson (Value (Number))
import "aeson" Data.Aeson.KeyMap (fromList)
import "data-default" Data.Default (Default, def)
import Scenario3_EmissionsTracking.Model
import Simulation
import Types


instance Default Plants where
  def = Plant


instance Default Beans where
  def = Bean


instance Default Milk where
  def = Oat


initialWorld :: World
initialWorld = World startingResources businesses []
  where
    businesses =
      -- [ SomeBusiness "flora's flowers" florist
      -- , SomeBusiness "haskell's cafe" cafe
      [ SomeBusiness "ivory's garden center" gardenCenter
      ]


spin :: World -> World
spin w@World{businesses} = newWorld
  where
    newOutputs :: WorldState [BusinessOutput]
    newOutputs = forM businesses (runProduction . run)

    newWorld = execWorldState newOutputs w

    run :: SomeBusiness -> ProductionState BusinessOutput
    run (SomeBusiness _ (f :: a -> ProductionState b))
      = BusinessOutput <$> f (def @a)


scenario3 :: IO ()
scenario3 = simulate "3. Emissions tracking" initialWorld 51 20




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
