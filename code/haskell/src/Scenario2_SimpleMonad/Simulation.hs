{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scenario2_SimpleMonad.Simulation where

import "base" Control.Monad (forM)
import "aeson" Data.Aeson (Value (Number))
import "aeson" Data.Aeson.KeyMap (fromList)
import "data-default" Data.Default (Default, def)
import "base" Data.Monoid (Sum (..))
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
initialWorld = World (Sum startingResources) businesses []
  where
    businesses
      = [ SomeBusiness "flora's flowers" florist
        , SomeBusiness "haskell's cafe"  cafe
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

-- TODO:
--  [ ] Think about the model of outputs; we keep them in the return type,
--      basically, _and_ as a list of outputs; so we're kind of duplicating them.
--      Maybe this could be cleaned up; or maybe it's not important. Amusingly,
--      linear types could be interesting here; but I don't think I'll do that.


instance SomeWorld World where
  spinWorld = spin
  someResources = resources
  otherPropertes w
    = fromList [ ("Resources", Number $ fromInteger $ getSum $ resources w)
               , ("Business Outputs", Number $ fromInteger $ toInteger $ length $ outputs $ w)
               ]


scenario2 :: IO ()
scenario2 = simulate initialWorld 51 20


reset :: IO ()
reset = simulate initialWorld 1 1
