module Types where

import "aeson" Data.Aeson (Object)

-- Common datatypes for our businesses

-- For a florist:

data Plants  = Plant
data Flowers = Flower


-- For a coffee shop:

data Beans  = Bean
data Milk   = Oat
data Coffee = Latte | Cappuccino


-- | What do businesses produce in this world? Why, Arbitrary Business Output,
-- of course!
data BusinessOutput = forall b. BusinessOutput b


-- | We only know that it was some output.
instance Show BusinessOutput where
  show _ = "Arbitrary Business Output"


-- | A convenient class to make it easier to render things.
class SomeWorld w where
  someResources  :: w -> Int
  otherPropertes :: w -> Object
  spinWorld      :: w -> w
