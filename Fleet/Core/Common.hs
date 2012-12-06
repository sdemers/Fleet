module Fleet.Core.Common where

import Debug.Trace as Dbg

type PlayerName = String

type Coord   = Float
data Point3D = Point3D { x, y, z :: Coord } deriving (Eq, Show)

origin3D = Point3D 0.0 0.0 0.0

translate (Point3D x y z) (Point3D offx offy offz) =
    Point3D (x + offx) (y + offy) (z + offz)

trace :: String -> a -> a
trace s a = Dbg.trace s a
--trace s a = a
