module Fleet.Comm.Radio where

type Frequency = Double

data RadioStatus = RadioOn | RadioOff | RadioDefective
                   deriving (Eq, Show)

data Radio = Radio {
    radioFrequency :: Double,
    radioStatus :: RadioStatus
} deriving (Show)
