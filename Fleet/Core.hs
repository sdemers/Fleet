module Fleet.Core
(
    mainloop,
    Simulator (dispatchMessages, simulate, printSim)
)
where

import Control.Concurrent
import Data.IORef
import Fleet.Comm.Message

import Debug.Trace

class Simulator s where
    dispatchMessages :: s -> IO ()
    printSim :: s -> IO ()

    -- | Simulates for an amount of time
    -- simulate :: s -> IO (IORef Integer) -> s
    simulate :: s -> Int -> IO ()

-- mainloop
mainloop simulator uptime msecResolution = do
    threadDelay (msecResolution * 1000)
    modifyIORef uptime (+msecResolution)
    putStrLn "Before simulation cycle"
    printSim simulator
    dispatchMessages simulator
    simulate simulator msecResolution
    putStrLn ""
    putStrLn "After simulation cycle"
    printSim simulator
    putStrLn "-------------------------------------------------------"
    mainloop simulator uptime msecResolution
