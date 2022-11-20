module Simulation where

import "base" Control.Monad (void, foldM)
import "base" Control.Concurrent (threadDelay)
import "aeson" Data.Aeson (Object, ToJSON)
import "base" Data.Monoid (Sum (..))
import "base" GHC.Generics (Generic)
import Mqtt
import Types


-- | A bit of a delay so the animation looks smooth.
sleep :: Int -> IO ()
sleep n = threadDelay (10_000 * n)


-- | The animations are all based of this number, so we centralise it.
startingResources :: Integer
startingResources = 100


-- | The update that's actually sent over MQTT.
data EarthUpdate = EarthUpdate
  { factor :: Double
  , step   :: Int
  , blob   :: Object
  }
  deriving (Generic, Show, ToJSON)


-- | Convert an arbitrary world to an update; really we just look at the
-- resources and compute the magic number from it; but we also send through
-- the other properties so it shows up on the UI.
toMqtt :: SomeWorld world => world -> Int -> EarthUpdate
toMqtt world i = EarthUpdate
  { factor = 1 - fromInteger (getSum $ someResources world)
           / (fromInteger startingResources)
  , step = i - 1
  , blob = otherPropertes world
  }


-- | Step through a series of spins of the world, rendering each one.
simulate :: SomeWorld world => world -> Int -> Int -> IO ()
simulate world n delay = do
  withMqtt $ \mc -> do
    void $ foldM (step mc) world [1 .. n]
  where
    step mc w i = do
      let w' = spinWorld w
      send mc (toMqtt w i)
      sleep delay
      pure w'
