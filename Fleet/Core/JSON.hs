module Fleet.Core.JSON
(
    JSONParsable(..),
    resultToMaybe
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

