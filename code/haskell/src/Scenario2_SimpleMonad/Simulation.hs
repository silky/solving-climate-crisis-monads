{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario2_SimpleMonad.Simulation where

import "base" Control.Monad (forM)
import "aeson" Data.Aeson (Value (Number))
import "aeson" Data.Aeson.KeyMap (fromList)
import "data-default" Data.Default (Default, def)
import Scenario2_SimpleMonad.Model
import Simulation
import Types


instance Default Plants where
  def = Plant


instance Default Beans where
  def = Bean


instance Default Milk where
  def = Oat


initialWorld :: World
initialWorld = World startingResources businesses []
  where
    businesses
      = [ SomeBusiness "flora's flowers" florist
        , SomeBusiness "haskell's cafe" cafe
        , SomeBusiness "ivory's garden center" gardenCenter
        ]


spin :: World -> World
spin w@World{businesses} = newWorld
  where
    newOutputs :: WorldState [BusinessOutput]
    newOutputs = forM businesses run

    newWorld = execWorldState newOutputs w

    run :: SomeBusiness -> WorldState BusinessOutput
    run (SomeBusiness _ (f :: a -> WorldState b))
      = BusinessOutput <$> f (def @a)


scenario2 :: IO ()
scenario2 = simulate "Simple monad" initialWorld 51 20


-- More businesses just consumes the resources faster:

scenario2a :: IO ()
scenario2a = simulate "More businesses" w 51 20
  where
    fs = repeat $ SomeBusiness "clover's clovers" florist
    n  = 1
    w  = initialWorld
          { businesses = take n fs <> businesses initialWorld
          }


-- In the present setup, you don't actually have to follow the rules:

rogueFlorist :: Plants -> WorldState Flowers
rogueFlorist = const . mkWorldState $
    \w -> (Just output, trackProduction w cost' output)
  where
    cost'  = cost @(Plants -> WorldState Flowers)
    output = Flower


scenario2b :: IO ()
scenario2b = simulate "Rogue operator" w 51 20
  where
    f = SomeBusiness "winterkorn's wildflowers" rogueFlorist
    w = initialWorld
          { businesses = f : businesses initialWorld
          }

-- Could fix the above via "regulation"; i.e. control which constructors are
-- exported.




-- Busywork for rendering

reset :: IO ()
reset = simulate "" initialWorld 1 1


instance SomeWorld World where
  spinWorld = spin
  someResources = resources
  otherPropertes w
    = fromList [ ("Resources", Number . fromIntegral . resources $ w)
               , ("Business Outputs", Number . fromIntegral . length . outputs $ w)
               ]

