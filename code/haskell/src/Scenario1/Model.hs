{-# language FlexibleInstances   #-}
{-# language NamedFieldPuns      #-}
{-# language FlexibleContexts    #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving  #-}

module Scenario1.Model where

import "data-default" Data.Default (def)
import "base" Data.Monoid (Sum (..))
import Scenario1.Types

data Plants  = Plant
data Flowers = Flower

florist :: Plants -> Flowers
florist = const Flower

data Beans  = Bean
data Milk   = Oat
data Coffee = Latte | Cappuccino

cafe :: (Beans, Milk) -> Coffee
cafe = const Latte

cost' :: SomeBusiness -> Sum Integer
cost' (SomeBusiness _ (_ :: a -> b)) = cost @(a -> b)


output :: SomeBusiness -> BusinessOutput
output (SomeBusiness _ (f :: a -> b)) = BusinessOutput $ f (def @a)
