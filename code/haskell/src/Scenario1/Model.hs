module Scenario1.Model where

data Plants  = Plant
data Flowers = Flower

florist :: Plants -> Flowers
florist = const Flower


data Beans  = Bean
data Milk   = Oat
data Coffee = Latte | Cappuccino

cafe :: (Beans, Milk) -> Coffee
cafe = const Latte
