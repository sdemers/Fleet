module Fleet.Core.Common where

import Debug.Trace as Dbg
import Fleet.Core.JSON
import Text.JSON
import Control.Applicative

type PlayerName = String

type Coord   = Float
data Point3D = Point3D { x, y, z :: Coord } deriving (Eq, Show)

instance JSON Point3D  where
    showJSON (Point3D x y z) = makeObj [("x", showJSON x), ("y", showJSON y), ("z", showJSON z)]
    readJSON (JSObject obj) = Point3D <$> f "x" <*> f "y" <*> f "z"
        where
            f x = valFromObj x obj

instance JSONParsable Point3D where
    parseJSON s = resultToMaybe $ decode s

origin3D = Point3D 0.0 0.0 0.0

translate (Point3D x y z) (Point3D offx offy offz) =
    Point3D (x + offx) (y + offy) (z + offz)

trace :: String -> a -> a
trace s a = Dbg.trace s a
--trace s a = a
