module Tag where

newtype Tag = Tag String deriving (Show)

new :: String -> Maybe Tag
new tag
    | null tag = Nothing
    | otherwise = Just $ Tag tag