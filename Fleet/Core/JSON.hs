module Fleet.Core.JSON
(
    JSONParsable(..),
    resultToMaybe,
    isResult
)
where

import Text.JSON

class JSONParsable a where
    parseJSON :: String -> Maybe a

-- Example
-- instance JSONParsable Point3D where
--     parseJSON s = resultToMaybe $ decode s

eitherToMaybe = either (const Nothing) Just
resultToMaybe = eitherToMaybe . resultToEither

isResult (Error _) = False
isResult _         = True

