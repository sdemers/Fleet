module Fleet.Spatial.Cartesian
where

import Fleet.Core.JSON
import Text.JSON
import Control.Applicative

data Point3D = Point3D {
    getX :: Double,
    getY :: Double,
    getZ :: Double
} deriving (Eq, Show)

instance JSON Point3D  where
    showJSON (Point3D x y z) = makeObj [("x", showJSON x),
                                        ("y", showJSON y),
                                        ("z", showJSON z)]
    readJSON (JSObject obj) = Point3D <$> f "x" <*> f "y" <*> f "z"
        where
            f x = valFromObj x obj

origin3D = Point3D 0.0 0.0 0.0

translate (Point3D x y z) (Point3D offx offy offz) =
    Point3D (x + offx) (y + offy) (z + offz)

