module Fleet.Comm.Message where

import Fleet.Comm.Radio
import Fleet.Core.Common
import Fleet.Core.JSON
import Text.JSON
import Control.Applicative
import Data.List
import Data.Maybe

-- | Message
data Message = Message {
    messageFrequency :: Frequency,
    messageRecipients :: [PlayerName],
    messageBody :: MessageBody
} deriving (Eq, Show)

instance JSON Message where
    showJSON = showJSONMessage
    readJSON (JSObject o) =
        case "messageId" `lookup` (fromJSObject o) of
            Nothing -> Error "Unable to read Message"
            Just s  -> readBody o s

readBody :: JSObject JSValue -> JSValue -> Result Message
readBody o (JSString s) = buildMessage <*> f "body"
    where
        buildMessage = Message <$> f "frequency" <*> f "recipients"
        f x = valFromObj x o

showJSONMessage (Message f px (InitPos m)) = showM f px "initialPosition" m
showJSONMessage (Message f px _)           = undefined
showM f px name body =
    makeObj [("messageId",  showJSON name),
             ("frequency",  showJSON f),
             ("recipients", showJSON px),
             ("body",       showJSON body)]

instance JSONParsable Message where
    parseJSON s = resultToMaybe $ decode s

data MessageBody = InitPos     MessageInitPos |
                   TargetSpeed MessageTargetSpeed
                   deriving (Eq, Show)

instance JSON MessageBody where
    showJSON (InitPos x)      = makeObj [("initialPosition", showJSON x)]
    showJSON _                = undefined

    readJSON (JSObject obj) =
        case find isJust table of
            Just a  -> readJSON $ fromJust a
            Nothing -> Error "Unable to read MessageBody"
        where
            table = map (flip lookup (fromJSObject obj)) messageTable
            messageTable = ["initialPosition", "toto"]

instance JSONParsable MessageBody where
    parseJSON s = resultToMaybe $ decode s

-- | MessageInitPos
data MessageInitPos = MessageInitPos {
    initPos :: Point3D
} deriving (Eq, Show)

instance JSON MessageInitPos where
    showJSON (MessageInitPos initPos) =
        makeObj [("position", showJSON initPos)]
    readJSON (JSObject obj) = MessageInitPos <$> f "position"
        where f x = valFromObj x obj

instance JSONParsable MessageInitPos where
    parseJSON s = resultToMaybe $ decode s

-- {"position":{"x":0.0,"y":0.1,"z":0.2}}

showInitPos = encode $ MessageInitPos (Point3D 0.1 0.2 0.3)

makeInitPosMessage p = InitPos $ MessageInitPos p

-- | MessageTargetSpeed
data MessageTargetSpeed = MessageTargetSpeed {
    targetSpeed :: Double
} deriving (Eq, Show)

makeTargetSpeedMessage s = TargetSpeed $ MessageTargetSpeed s
