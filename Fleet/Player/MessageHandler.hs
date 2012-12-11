module Fleet.Player.MessageHandler
(
    handleMessage
)
where

import Fleet.Core.Common
import Fleet.Player
import Fleet.Comm.Radio
import Fleet.Comm.Message

handleMessage :: Player -> Message -> (Player, [Message])
handleMessage p m
    | checkRadio p m = acceptMessage p (messageBody m)
    | otherwise      = (p, [])

checkRadio p m
    | not radioOn        = False
    | not radioFreqValid = False
    | not isRecipient    = False
    | otherwise          = True
    where radio   = getRadio p
          radioOn = (radioStatus radio) == RadioOn
          radioFreqValid = (radioFrequency radio) == (messageFrequency m)
          isRecipient = (getName p) `elem` (messageRecipients m)

newPos p = Message 123.4 ["Serge"] (makeInitPosMessage (translate p (Point3D 1.0 1.0 1.0)))

acceptMessage :: Player -> MessageBody -> (Player, [Message])
acceptMessage p (InitPos m) = ((setPosition p (initPos m)), [])
