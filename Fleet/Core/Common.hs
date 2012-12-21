module Fleet.Core.Common where

import Debug.Trace as Dbg

type PlayerName = String

trace :: String -> a -> a
trace s a = Dbg.trace s a
--trace s a = a
