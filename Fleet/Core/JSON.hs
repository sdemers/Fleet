module Fleet.Core.JSON
(
    JSONParsable(..),
    resultToMaybe,
    isResult,
    textToJSONs
)
where

import Text.JSON
import Text.Regex

class JSONParsable a where
    parseJSON :: String -> Maybe a

-- Example
-- instance JSONParsable Point3D where
--     parseJSON s = resultToMaybe $ decode s

eitherToMaybe = either (const Nothing) Just
resultToMaybe = eitherToMaybe . resultToEither

isResult (Error _) = False
isResult _         = True

-- | Splits text into a list of JSON-encoded records
-- Example:
-- "{ \"number\": 1 }{ \"number\": 2} gives
-- ["{\"number\": 1}", "{\"number\": 2}"]
textToJSONs text = splitRegex regex mod
    where
        regex = mkRegex "\\}[\\t \\n]*\\{"
        mod = subRegex regex text "}} {{"
