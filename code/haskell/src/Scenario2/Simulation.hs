{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario2.Simulation where

import "data-default" Data.Default (Default, def)
import "base" Data.Monoid (Sum (..))
import Scenario2.Model
import Scenario2.Types
import Scenario1.Simulation (simulate, initialResources)

instance Default Plants where
  def = Plant

instance Default Beans where
  def = Bean

instance Default Milk where
  def = Oat

initialWorld :: World
initialWorld = World (Sum initialResources) businesses []
  where
    businesses
      = [ SomeBusiness "flora's flowers" florist
        , SomeBusiness "haskell's cafe"  cafe
        ]


spin :: World -> World
spin w@World{businesses} = newWorld
  where
    newOutputs :: WorldState [BusinessOutput]
    newOutputs = sequence $ map run businesses

    newWorld = snd $ runWorld newOutputs w

    run :: SomeBusiness -> WorldState BusinessOutput
    run (SomeBusiness _ (f :: a -> WorldState b)) = BusinessOutput <$> f (def @a)
