module Fleet.Comm.Message
(
    Message(..),
    SystemMessage(..),
    PlayerMessage(..),
    SystemMessageBody(..),
    MessageNewPlayer(..),
    PlayerMessageBody(..),
    MessageInitPos(..),
    MessageTargetSpeed(..),
    parseMessage,

    newPlayerMess,
    initPosMess
)
where

import Fleet.Comm.Radio
import Fleet.Core.Common
import Fleet.Spatial.Cartesian
import Fleet.Core.JSON
import Text.JSON
import Control.Applicative
import Data.List(find)

data Message = SysMessage SystemMessage |
               PlayMessage PlayerMessage
               deriving (Eq, Show)

instance JSON Message where
    showJSON (SysMessage m)  = makeObj [("systemMessage", showJSON m)]
    showJSON (PlayMessage m) = makeObj [("playerMessage", showJSON m)]
    readJSON (JSObject o) =
            case find isResult (applyMessageTable o buildMessageTable) of
                Nothing -> Error "Message not found"
                Just a  -> a

buildMessageTable = [(buildMessage SysMessage "systemMessage"),
                     (buildMessage PlayMessage "playerMessage")]

-- | SystemMessage
data SystemMessage = SystemMessage {
    sysMessageBody :: SystemMessageBody
} deriving (Eq, Show)

instance JSON SystemMessage where
    readJSON (JSObject o) = SystemMessage <$> f "body"
        where
            f x = valFromObj x o
    showJSON (SystemMessage b) = makeObj [("body",  showJSON b)]

data SystemMessageBody =
    NewPlayer MessageNewPlayer
    deriving (Eq, Show)

-- | SystemMessageBody instances
-- No need to add showJSON instances since we won't use them
-- As for readJSON, we only need to add them to messageTable
instance JSON SystemMessageBody where
    showJSON (NewPlayer m) = makeObj [("newPlayer", showJSON m)]
    readJSON (JSObject o) =
            case find isResult (applyMessageTable o buildSysTable) of
                Nothing -> Error "SystemMessageBody not found"
                Just a  -> a

buildSysTable = [(buildMessage NewPlayer "newPlayer")]

data MessageNewPlayer = MessageNewPlayer {
    npName :: String,
    npType :: String, -- "pilot", "driver"
    npFrequency :: Double
} deriving (Eq, Show)

instance JSON MessageNewPlayer where
    showJSON (MessageNewPlayer n t f) =
        makeObj [("name", showJSON n), ("type", showJSON t), ("frequency", showJSON f)]
    readJSON (JSObject o) = MessageNewPlayer <$> f "name" <*> f "type" <*> f "frequency"
        where
            f x = valFromObj x o

-- | PlayerMessage
data PlayerMessage = PlayerMessage {
    plMessageFrequency :: Frequency,
    plMessageRecipients :: [PlayerName],
    plMessageBody :: PlayerMessageBody
} deriving (Eq, Show)

instance JSON PlayerMessage where
    readJSON (JSObject o) =
        PlayerMessage <$> f "frequency" <*> f "recipients" <*> f "body"
        where
            f x = valFromObj x o
    showJSON (PlayerMessage f r b) =
        makeObj [("frequency",  showJSON f),
                 ("recipients", showJSON r),
                 ("body", showJSON b)]

-- | All message constructors shall be declared here
data PlayerMessageBody =
    InitPos     MessageInitPos |
    TargetSpeed MessageTargetSpeed
    deriving (Eq, Show)

-- | PlayerMessageBody instances
-- No need to add showJSON instances since we won't use them
-- As for readJSON, we only need to add them to messageTable
instance JSON PlayerMessageBody where
    showJSON (InitPos x) = makeObj [("initialPosition", showJSON x)]
    showJSON _           = undefined

    readJSON (JSObject o) =
            case find isResult (applyMessageTable o buildPlayerTable) of
                Nothing -> Error "PlayerMessageBody not found"
                Just a  -> a

buildPlayerTable = [(buildMessage InitPos "initialPosition"),
                    (buildMessage TargetSpeed "targetSpeed")]

buildMessage c m o = c <$> (valFromObj m o)

applyMessageTable o mt = map (\f -> f o) mt

-- | Parses a JSON-formatted string and returns Maybe Message
parseMessage m = resultToMaybe res
    where res = decode m :: Result Message

-- Example encoded message (initial position)
-- {
--     "frequency":123.4,
--     "recipients":["Serge"],
--     "body": {
--         "initialPosition": {
--             "position": { "x":0.1, "y":0.2, "z":0.3 }
--         }
--     }
-- }

newPlayerMess = encode $ SysMessage $ SystemMessage (NewPlayer $ MessageNewPlayer "Serge" "pilot" 123.4)
initPosMess = encode $ PlayMessage $
    PlayerMessage 123.4 ["Serge"] (InitPos $ MessageInitPos (Point3D 0.0 0.0 0.0))

-- | MessageInitPos
data MessageInitPos = MessageInitPos {
    initPos :: Point3D
} deriving (Eq, Show)

instance JSON MessageInitPos where
    showJSON (MessageInitPos initPos) = makeObj [("position", showJSON initPos)]
    readJSON (JSObject obj) = MessageInitPos <$> f "position"
        where f x = valFromObj x obj

-- | MessageTargetSpeed
data MessageTargetSpeed = MessageTargetSpeed {
    targetSpeed :: Double
} deriving (Eq, Show)

instance JSON MessageTargetSpeed where
    showJSON = undefined
    readJSON = undefined

