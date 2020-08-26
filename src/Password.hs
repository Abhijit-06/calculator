module Password where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

newtype Password = Password
    { unPassword :: ByteString
    } deriving (Show)

new :: ByteString -> Maybe Password
new pwd
    | ByteString.null pwd = Nothing
    | otherwise = Just (Password pwd)

newUnsafe :: ByteString -> Password
newUnsafe = Password