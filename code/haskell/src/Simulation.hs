module Simulation where

import "base" Control.Concurrent (threadDelay)
import "base" Control.Monad (foldM, void)
import "aeson" Data.Aeson (Object, ToJSON, Value (String, Number))
import "aeson" Data.Aeson.KeyMap (fromList)
import "base" GHC.Generics (Generic)
import "text" Data.Text (Text)
import Mqtt
import Types


-- | A bit of a delay so the animation looks smooth.
sleep :: Int -> IO ()
sleep n = threadDelay (10_000 * n)


-- | The animations are all based of this number, so we centralise it.
startingResources :: Int
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
toMqtt :: SomeWorld world => Text -> world -> Int -> EarthUpdate
toMqtt name world i = EarthUpdate
  { factor = 1 - fromIntegral (someResources world)
           / (fromIntegral startingResources)
  , step = i - 1
  , blob = otherPropertes world <> extraProps
  }
    where
      extraProps = fromList [ ("* Model", String name)
                            , ("* Step #", Number $ fromIntegral i)
                            ]



-- | Step through a series of spins of the world, rendering each one.
simulate :: SomeWorld world => Text -> world -> Int -> Int -> IO ()
simulate name world n delay = do
  withMqtt $ \mc -> do
    void $ foldM (step mc) world [1 .. n]
  where
    step mc w i = do
      let w' = spinWorld w
      send mc (toMqtt name w i)
      sleep delay
      pure w'
