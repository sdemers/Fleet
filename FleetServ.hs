{-# LANGUAGE FlexibleInstances #-}

import Data.IORef
import Data.List
import Data.Maybe

import Control.Concurrent
import Control.Monad (forever)

import Fleet.Core
import Fleet.Core.JSON
import Fleet.Core.Common
import Fleet.Core.Listener
import Fleet.Player
import Fleet.Player.MessageHandler
import Fleet.Comm.Message
import Fleet.Comm.Radio

-- Simulation resolution (100 msec)
resolutionMs = 1000
resolution = resolutionMs * 1000

data FleetSim = FleetSim {
    messages :: [Message],
    players :: [Player]
} deriving (Show)

--instance Simulator FleetSim where
instance Simulator (IORef FleetSim) where

    dispatchMessages s = modifyIORef s dispatchSimMessages

    simulate s t = do
        newSim <- readIORef s
        return ()

    printSim s = do
        sim <- readIORef s
        print ("Sim: " ++ show sim)

dispatchSimMessages s = dispatchMessageList (messages s) (players s)

dispatchMessageList :: [Message] -> [Player] -> FleetSim
dispatchMessageList m p  = FleetSim newMessages newPlayers
    where
        newPlayers   = catMaybes maybeSysPlayers ++ catMaybes maybePlayers
        newMessages  = nub $ concat (newPlayerMessages ++ newSysMessages)
        (maybePlayers, newPlayerMessages) = unzip $ dispatchToPlayers p m
        (maybeSysPlayers, newSysMessages) = unzip $ dispatchSystemMessages m

dispatchToPlayers :: [Player] -> [Message] -> [(Maybe Player, [Message])]
dispatchToPlayers (pl:ps) ms = (dispatchToPlayer pl ms : dispatchToPlayers ps ms)
dispatchToPlayers [] _  = [(Nothing, [])]

dispatchToPlayer :: Player -> [Message] -> (Maybe Player, [Message])
dispatchToPlayer p mx = foldl applyMessage (Just p, []) mx

applyMessage (Just player, initMessages) m = (Just newPlayer, newMessages)
    where
        (newPlayer, retMessages) = handlePlayerMessage player m
        newMessages              = initMessages ++ retMessages

dispatchSystemMessages :: [Message] -> [(Maybe Player, [Message])]
dispatchSystemMessages (m:ms) = (handleSystemMessage m : dispatchSystemMessages ms)
dispatchSystemMessages [] = [(Nothing, [])]

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = putStrLn $ "From " ++ show addr ++ ": " ++ msg

-- Handler that receives and parses messages and adds them to the queue
jsonHandler :: IORef FleetSim -> HandlerFunc
jsonHandler simulator addr text = trace text $ modifyIORef simulator (addMessages newMessages)
    where
        newMessages = catMaybes $ map parseMessage (textToJSONs text)
        addMessages msgs sim = FleetSim ((messages sim) ++ msgs) (players sim)

main = do
    simulator <- newIORef (FleetSim [] [])
    listenerId <- forkIO $ startListener "5000" (jsonHandler simulator)
    uptime <- newIORef 0
    forever $ mainloop simulator uptime resolutionMs
