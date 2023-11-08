module Javascript where

import Prelude

foreign import fromStringToBase64String :: String -> String

-- | depth:
-- |   - 1: monochrome
-- |   - 4: 4-bit grayscale
-- |   - 8: 8-bit grayscale
-- |   - 16: 16-bit colour
-- |   - 32: 32-bit colour
foreign import fromByteArrayToImageSrc :: Array Int -> Int -> String

foreign import exampleImageSrc :: Unit -> String

