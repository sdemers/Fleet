{-# LANGUAGE FlexibleInstances #-}

import Data.IORef
import Data.List
import Data.Maybe

import Control.Concurrent

import Fleet.Core
import Fleet.Core.Common
import Fleet.Core.Listener
import Fleet.Player
import Fleet.Player.MessageHandler
import Fleet.Comm.Message
import Fleet.Comm.Radio

-- Simulation resolution (100 msec)
resolutionMs = 100
resolution = resolutionMs * 1000

data FleetSim = FleetSim {
    messages :: [Message],
    players :: [Player]
} deriving (Show)

makePilots = [makePilot "Serge" (Radio 123.4 RadioOn)]

initialMessages = [Message 123.4 ["Serge"] (InitPos $ MessageInitPos (Point3D 1.0 2.0 3.0))]

--instance Simulator FleetSim where
instance Simulator (IORef FleetSim) where

    dispatchMessages s = modifyIORef s dispatchSimMessages

    simulate s t = do
        newSim <- readIORef s
        print newSim

    printSim s = do
        sim <- readIORef s
        print ("Sim: " ++ show (players sim))

dispatchSimMessages s = dispatchMessageList (messages s) (players s)

dispatchMessageList :: [Message] -> [Player] -> FleetSim
dispatchMessageList m p  = FleetSim newMessages newPlayers
    where
        newMessages  = nub $ concat allNewMessages
        newPlayers   = catMaybes maybePlayers
        (maybePlayers, allNewMessages) = unzip $ dispatchToPlayers p m

dispatchToPlayers :: [Player] -> [Message] -> [(Maybe Player, [Message])]
dispatchToPlayers (pl:ps) ms = (dispatchToPlayer pl ms : dispatchToPlayers ps ms)
dispatchToPlayers [] _  = [(Nothing, [])]

dispatchToPlayer :: Player -> [Message] -> (Maybe Player, [Message])
dispatchToPlayer p mx = foldl applyMessage (Just p, []) mx

applyMessage (Just player, initMessages) m = (Just newPlayer, newMessages)
    where
        (newPlayer, retMessages) = handleMessage player m
        newMessages              = initMessages ++ retMessages

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = putStrLn $ "From " ++ show addr ++ ": " ++ msg

main = do
    listenerId <- forkIO $ startListener "5000" plainHandler
    simulator <- newIORef (FleetSim initialMessages makePilots)
    uptime <- newIORef 0
    mainloop simulator uptime resolutionMs
    print "Done."
