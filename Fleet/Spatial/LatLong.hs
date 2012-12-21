module Fleet.Spatial.LatLong
(
    LatLong,
    getLat,
    getLong,
    getSinLat,
    getSinLong,
    getCosLat,
    getCosLong,
    makeLatLong,
    makeLat,
    makeLong,
    normTo2Pi,
    normToPi,
    normToHalfPi,
    getBearing
)
where

type Radian = Double

makeRadian rad = normTo2Pi rad
makeLat    rad = normToHalfPi rad
makeLong   rad = normToPi rad

data LatLong = LatLongCons {
    getLat :: Radian,
    getLong :: Radian,
    getSinLat :: Double,
    getCosLat :: Double,
    getSinLong :: Double,
    getCosLong :: Double
} deriving (Eq, Show)

makeLatLong lat long =
    LatLongCons normLat normLong sinLat cosLat sinLong cosLong
    where
        normLat  = makeLat lat
        normLong = makeLong long
        sinLat   = sin normLat
        cosLat   = cos normLat
        sinLong  = sin normLong
        cosLong  = cos normLong

-- | Normalize between 0 and 2pi
normTo2Pi rad = normGeneric rad 0.0 (2*pi)

-- | Normalize between -pi and pi
normToPi rad = normGeneric rad (-pi) pi

-- | Normalize between 0 and pi/2
normToHalfPi rad = normGeneric rad 0.0 (pi/2.0)

normGeneric rad low high
    | rad > high = normGeneric (rad - dist) low high
    | rad < low  = normGeneric (rad + dist) low high
    | otherwise  = rad
    where dist = high - low

getBearing a b = normTo2Pi bearing
    where
        bearing = atan2 y x
        x       = ((getCosLat a) * (getSinLat b)) -
                  ((getSinLat a) * (getCosLat b)) * (cos diffLon)
        y       = (sin diffLon) * (getCosLat b)
        diffLon = (getLong b) - (getLong a)
