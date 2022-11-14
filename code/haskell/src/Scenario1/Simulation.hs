{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario1.Simulation where

import "base" Control.Monad (foldM, void)
import "aeson" Data.Aeson (Value(Number), Object, ToJSON)
import "aeson" Data.Aeson.KeyMap (fromList)
import "data-default" Data.Default (Default, def)
import "base" Data.Monoid (Sum (..))
import "base" GHC.Generics (Generic)
import Misc (sleep)
import Mqtt (send, withMqtt)
import Scenario1.Model
import Scenario1.Types


-- Below we pay the price for not modelling our domain properly; we define a
-- bunch of orphan instances.

instance Cost (Plants -> Flowers) where
  cost = Sum 1

instance Cost ((Beans, Milk) -> Coffee) where
  cost = Sum 1

instance Default Plants where
  def = Plant

instance Default Beans where
  def = Bean

instance Default Milk where
  def = Oat



-- | Every time the world spins, the following events occur:
--    - Businesses produce outputs.
--    - The amount of resources reduces precisely by the cost of operating businesses.
--    - The amount of businesses stays the same.
spin :: World -> World
spin World{resources, businesses, outputs} =
  World
    { resources  = resources - resourceCosts
    , outputs    = newOutputs ++ outputs
    , businesses = businesses
    }
  where
    resourceCosts = foldMap cost' businesses
    newOutputs    = map output businesses

    -- Because of the way our representation works, we need to pattern-match to
    -- call our functions at the right type.

    cost' :: SomeBusiness -> Sum Integer
    cost' (SomeBusiness _ (_ :: a -> b)) = cost @(a -> b)

    output :: SomeBusiness -> BusinessOutput
    output (SomeBusiness _ (f :: a -> b)) = BusinessOutput $ f (def @a)


-- We can now define an initial world state, and simulate it.


initialWorld :: World
initialWorld = World (Sum initialResources) businesses []
  where
    businesses
      = [ SomeBusiness "flora's flowers" florist
        , SomeBusiness "haskell cafe"    cafe
        ]

initialResources :: Integer
initialResources = 100



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


scenario1 :: IO ()
scenario1 = simulate initialWorld 51 20
