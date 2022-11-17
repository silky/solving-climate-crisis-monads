{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario2.Simulation where

import "data-default" Data.Default (Default, def)
import "base" Data.Monoid (Sum (..))
import Scenario2.Model
import Scenario2.Types
import Scenario1.Simulation (simulate, initialResources)

initialWorld :: World
initialWorld = World (Sum initialResources) businesses []
  where
    businesses
      = [ SomeBusiness "flora's flowers" florist
        ]


spin :: World -> World
spin w@World{resources,businesses,outputs} = undefined
  where
    -- ms = map (flip runWorld w) businesses
    -- costs = foldMap cost' businesses

    -- cost' :: SomeBusiness -> Sum Integer
    -- cost' (SomeBusiness _ (_ :: a -> b)) = cost @(a -> b)

    output :: SomeBusiness -> BusinessOutput
    output (SomeBusiness _ (f :: a -> b)) =
      let o = f (def @a)
       in BusinessOutput $ runWorld o w
