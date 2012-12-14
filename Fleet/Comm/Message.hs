module Fleet.Comm.Message
(
    Message(..),
    MessageBody(..),
    MessageInitPos(..),
    MessageTargetSpeed(..),
    parseMessage
)
where

import Fleet.Comm.Radio
import Fleet.Core.Common
import Fleet.Core.JSON
import Text.JSON
import Control.Applicative
import Data.List(find)

-- | Message
data Message = Message {
    messageFrequency :: Frequency,
    messageRecipients :: [PlayerName],
    messageBody :: MessageBody
} deriving (Eq, Show)

-- | All message constructors shall be declared here
data MessageBody = InitPos     MessageInitPos |
                   TargetSpeed MessageTargetSpeed
                   deriving (Eq, Show)

instance JSON Message where
    readJSON (JSObject o) =
        Message <$> f "frequency" <*> f "recipients" <*> f "body"
        where
            f x = valFromObj x o
    showJSON (Message f r b) = makeObj [("frequency",  showJSON f),
                                        ("recipients", showJSON r),
                                        ("body", showJSON b)]

-- | MessageBody instances
-- No need to add showJSON instances since we won't use them
-- As for readJSON, we only need to add them to messageTable
instance JSON MessageBody where
    showJSON (InitPos x) = showJSON x
    showJSON _           = undefined

    readJSON (JSObject o) = buildMessageBody o

buildMessageBody o =
    case find isResult (applyBuildTable o) of
        Nothing -> Error "Error: MessageBody not found"
        Just a  -> a

buildTable = [(buildMessage InitPos "initialPosition"),
              (buildMessage TargetSpeed "targetSpeed")]

applyBuildTable o = map (\f -> f o) buildTable
buildMessage c m o = c <$> (valFromObj m o)

-- | Parses a JSON-formatted string and returns Maybe Message
parseMessage m = resultToMaybe $ (decode m :: Result Message)

-- | MessageInitPos
data MessageInitPos = MessageInitPos {
    initPos :: Point3D
} deriving (Eq, Show)

instance JSON MessageInitPos where
    showJSON (MessageInitPos initPos) =
        makeObj [("position", showJSON initPos)]
    readJSON (JSObject obj) = MessageInitPos <$> f "position"
        where f x = valFromObj x obj

-- Example encoded message
-- {
--     "frequency":123.4,
--     "recipients":["Serge"],
--     "body": {
--         "initialPosition": {
--             "position": { "x":0.1, "y":0.2, "z":0.3 }
--         }
--     }
-- }

showInitPos = encode $ Message 123.4 ["Serge"] (InitPos $ MessageInitPos (Point3D 0.1 0.2 0.3))
showTargetSpeed = encode $ Message 123.4 ["Serge"] (TargetSpeed $ MessageTargetSpeed 25.0)

encodedInitPos = "{\"frequency\":123.4,\"recipients\":[\"Serge\"],\"body\":{\"initialPosition\":{\"position\":{\"x\":0.1,\"y\":0.2,\"z\":0.3}}}}"
receivedInitPos = parseMessage encodedInitPos

-- | MessageTargetSpeed
data MessageTargetSpeed = MessageTargetSpeed {
    targetSpeed :: Double
} deriving (Eq, Show)

instance JSON MessageTargetSpeed where
    showJSON = undefined
    readJSON = undefined

