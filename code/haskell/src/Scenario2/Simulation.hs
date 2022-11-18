{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario2.Simulation where

import "base" Control.Monad (foldM, void)
import "aeson" Data.Aeson (Object, ToJSON, Value (Number))
import "aeson" Data.Aeson.KeyMap (fromList)
import "data-default" Data.Default (Default, def)
import "base" Data.Monoid (Sum (..))
import "base" GHC.Generics (Generic)
import Misc (sleep)
import Mqtt (send, withMqtt)
import Scenario1.Simulation (initialResources)
import Scenario2.Model
import Scenario2.Types

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
    run (SomeBusiness _ (f :: a -> WorldState b))
      = BusinessOutput <$> f (def @a)

-- TODO:
--  [ ] Think about the model of outputs; we keep them in the return type,
--      basically, _and_ as a list of outputs; so we're kind of duplicating them.
--      Maybe this could be cleaned up; or maybe it's not important. Amusingly,
--      linear types could be interesting here; but I don't think I'll do that.

-- TODO:
--  [ ] Refactor so these things can be called generally; i.e. so we're not just
--      copying-and-pasting it from Scenario1.

data EarthUpdate = EarthUpdate
  { step :: Double
  , blob :: Object
  }
  deriving (Generic, Show, ToJSON)


toMqtt :: World -> EarthUpdate
toMqtt World{resources,outputs} = EarthUpdate
  { step = 1 - fromInteger (getSum resources) / (fromInteger initialResources)
  , blob = fromList [ ("Resources", Number $ fromInteger $ getSum resources)
                    , ("Business Outputs", Number $ fromInteger $ toInteger $ length $ outputs)
                    ]
  }


simulate :: World -> Int -> Int -> IO ()
simulate world n delay = do
  withMqtt $ \mc -> do
    void $ foldM (step mc) world [1 .. n]
  where
    step mc w _ = do
      let w' = spin w
      send mc (toMqtt w)
      sleep delay
      pure w'


scenario2 :: IO ()
scenario2 = simulate initialWorld 51 20

reset :: IO ()
reset = simulate initialWorld 1 1
