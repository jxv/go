module Go.MonteCarlo
    ( insideUnitCircle
    , sample
    ) where


import System.Random


insideUnitCircle :: (Floating a, Ord a) => a -> a -> Bool
insideUnitCircle x y = 1 >= (x^2 + y^2)


sample :: IO Bool
sample = do x <- randomIO :: IO Float
            y <- randomIO
            return $ insideUnitCircle x y
