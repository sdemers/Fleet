module Fleet.Comm.Message where

import Fleet.Comm.Radio
import Fleet.Core.Common

data Message = Message {
    messageFrequency :: Frequency,
    messageRecipients :: [PlayerName],
    messageBody :: MessageBody
} deriving (Eq, Show)

data MessageBody  = InitPos     MessageInitPos |
                    TargetSpeed MessageTargetSpeed
                    deriving (Eq, Show)

data MessageInitPos = MessageInitPos {
    initPos :: Point3D
} deriving (Eq, Show)

makeInitPosMessage p = InitPos $ MessageInitPos p

data MessageTargetSpeed = MessageTargetSpeed {
    targetSpeed :: Double
} deriving (Eq, Show)

makeTargetSpeedMessage s = TargetSpeed $ MessageTargetSpeed s
