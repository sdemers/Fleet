module Fleet.Player where

import Fleet.Comm.Radio
import Fleet.Core.Common

data Pilot = Pilot {
    pilotName :: String,
    pilotRadio :: Radio,
    pilotPosition :: Point3D
} deriving (Eq, Show)

data Driver = Driver {
    driverName :: String,
    driverRadio :: Radio
} deriving (Eq, Show)

data Player = SimPilot Pilot |
              SimDriver Driver
              deriving (Eq, Show)

makePilot name radio = SimPilot $ Pilot name radio origin3D
makeDriver name radio = SimDriver $ Driver name radio

getName (SimPilot a) = pilotName a
getName (SimDriver a) = driverName a

getRadio (SimPilot a) = pilotRadio a
getRadio (SimDriver a) = driverRadio a

setPosition :: Player -> Point3D -> Player
setPosition (SimPilot a) pos = SimPilot $ a { pilotPosition = pos }
setPosition p pos = p
