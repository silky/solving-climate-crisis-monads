module Types where

import "aeson" Data.Aeson (Object)

-- Common datatypes for our businesses

-- For a florist:

data Plants  = Plant
  deriving Show

data Flowers = Flower
  deriving Show


-- For a coffee shop:

data Beans  = Bean
  deriving Show

data Milk   = Oat
  deriving Show

data Coffee = Latte | Cappuccino
  deriving Show


-- For a garden center:

-- data GardenCenterInputs  = GCI Plants Beans Milk
-- data GardenCenterOutputs = GCO Flowers Coffee


-- | What do businesses produce in this world? Why, Arbitrary Business Output,
-- of course!
data BusinessOutput = forall b. BusinessOutput b


-- TODO: Could add a 'Show' instance to 'BusinessOutput' and use the show
-- instances per thing.


-- | We only know that it was some output.
instance Show BusinessOutput where
  show _ = "Arbitrary Business Output"


-- | A convenient class to make it easier to render things.
class SomeWorld w where
  someResources  :: w -> Int
  otherPropertes :: w -> Object
  spinWorld      :: w -> w
