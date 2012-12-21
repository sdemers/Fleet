module Fleet.Spatial.Coord
(
    Coord(..),
    coordToXYZ
)
where

import Fleet.Core.Conv
import Fleet.Spatial.LatLong
import Fleet.Spatial.Cartesian

earthRadiusInMeter = 6378.14 * 1000

data Coord = Coord {
    getLatLong :: LatLong,

    -- Above Sea Level
    getFeetASL :: Double
} deriving (Eq, Show)

-- | WGS-84 datum
getEarthRadiusAtLatLong ll = radiusInFeet
    where
        f = 1/298.257223563
        e2 = 2 * f - (f ^ 2)
        n = 6378137.0 / (sqrt $ 1 - (e2 * ((getSinLat ll) ^ 2)))
        x = n * (getCosLat ll) * (getCosLong ll)
        y = n * (getCosLat ll) * (getSinLong ll)
        z = (n * (1 - e2)) * (getSinLat ll)
        earthRadiusInKM = sqrt ((x^2)+ (y^2) + (z^2))
        radiusInFeet = kmToFeet earthRadiusInKM

coordToXYZ projCenter (Coord ll asl) = Point3D x y z
    where
        cosLongDiff = cos ((getLong ll) - (getLong projCenter))
        sinLongDiff = sin ((getLong ll) - (getLong projCenter))

        -- C, the angular distance of coord from the
        -- projection center.
        cosC = (getSinLat ll) * (getSinLat projCenter) +
               (getCosLat ll) * (getCosLat projCenter) * cosLongDiff

        radius = asl + getEarthRadiusAtLatLong ll

        (x, y, z)
            | cosC == 0.0 = (0.0, 0.0, 0.0)
            | otherwise   = (x', y', radius)
            where
                x' = (radius / cosC) * (getCosLat ll) * sinLongDiff
                y' = (radius / cosC) * ((getCosLat projCenter) * (getSinLat ll))
                                     - (getSinLat projCenter) * (getCosLat ll)
                                     * (cosLongDiff)

