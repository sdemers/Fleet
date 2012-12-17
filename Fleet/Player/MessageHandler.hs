module Fleet.Player.MessageHandler
(
    handlePlayerMessage,
    handleSystemMessage
)
where

import Fleet.Core.Common
import Fleet.Player
import Fleet.Comm.Radio
import Fleet.Comm.Message

handlePlayerMessage :: Player -> Message -> (Player, [Message])
handlePlayerMessage p (PlayMessage m)
    | checkRadio p m = acceptPlayerMessage p (plMessageBody m)
    | otherwise      = (p, [])

handlePlayerMessage p _ = undefined

handleSystemMessage :: Message -> (Maybe Player, [Message])
handleSystemMessage (SysMessage m) = acceptSysMessage (sysMessageBody m)
handleSystemMessage _ = (Nothing, [])


checkRadio p m
    | not radioOn        = False
    | not radioFreqValid = False
    | not isRecipient    = False
    | otherwise          = True
    where radio   = getRadio p
          radioOn = (radioStatus radio) == RadioOn
          radioFreqValid = (radioFrequency radio) == (plMessageFrequency m)
          isRecipient = (getName p) `elem` (plMessageRecipients m)

acceptPlayerMessage :: Player -> PlayerMessageBody -> (Player, [Message])
acceptPlayerMessage p (InitPos m) = ((setPosition p (initPos m)), [])

acceptSysMessage (NewPlayer (MessageNewPlayer name kind freq)) = (Just newPl, [])
    where newPl = case kind of
                     "pilot" -> makePilot name radio
                     _       -> makeDriver name radio
                  where radio = Radio freq RadioOn
