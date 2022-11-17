{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario2.Simulation where

import "data-default" Data.Default (Default, def)
import "base" Data.Monoid (Sum (..))
import Scenario2.Model
import Scenario2.Types

instance Cost (Plants -> Flowers) where
  cost = Sum 1

instance Default Plants where
  def = Plant
