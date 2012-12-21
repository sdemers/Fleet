module Fleet.Core.Conv
where

feetToMeter :: Floating a => a -> a
feetToMeter = (*) 0.3048

meterToFeet :: Eq a => Floating a => a -> a
meterToFeet a
    | a == 0.0  = 0.0
    | otherwise = 1.0 / feetToMeter a

feetToKm :: Eq a => Floating a => a -> a
feetToKm a = feetToMeter a / 1000.0
kmToFeet a
    | a == 0.0  = 0.0
    | otherwise = 1 / (feetToKm a)

msecToSec a = a / 1000
secToMsec a = a * 1000
