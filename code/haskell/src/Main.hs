module Main where


main :: IO ()
main = putStrLn "Nothing to see here."


-- TODO:
--  - [ ] Implement scenario 3; the `Production` Monad.
--
--  - [ ] Explain why we can't just limit production in Scenario1 in the
--        'simulate' function. (Why? Actually I don't know the answer.)
--
--  - [x] Work out a new way to compute the cost in the example of the
--        `Production` monad; maybe it relates to the above, where we _don't_ use
--        the instances initially to determine the cost. Maybe in Scenario1, we
--        don't have an explicit cost; or I guess we can but we do it in the
--        Simulation.
--        Maybe can distinguish three domains:
--
--          1. Businesses
--          2. Government
--          3. God/Simulation
--
--    The idea, then, can be to put more things out "our" control; or
--    something.
--
--
--  - [ ] Eventually want to get to a state where businesses are not only
--        producing _output_ but also doing some kind of regnerative work.
--
--  - [ ] In the `admin` section add flag to stop the earth rotating.
--
--  - [x] Complete example two showing how to deal with the rogue florist.
--  - [x] Change the `gardenCenter` example to also make some output of its
--        own? (Nah, not important.)
--  - [!] Try and use the StateT thing properly:
--        <https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html>
