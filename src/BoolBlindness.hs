module BoolBlindness where

import Data.IntMap (IntMap)
import Data.Maybe (fromJust)
import Data.Maybe (isNothing)

import qualified Data.IntMap as IntMap

-- EXAMPLE 1
-- Incorrect code
blindAdd :: (a -> Maybe Int) -> (a -> Maybe Int) -> a -> Maybe Int
blindAdd f g x =
    if isNothing (f x) || isNothing (g x)
    then Nothing
    else Just (fromJust (f x) + fromJust (g x))

-- Now the correct solution for this anti pattern
-- We make use of the applicative properties
add :: (a -> Maybe Int) -> (a -> Maybe Int) -> a -> (Maybe Int)
add f g x = (+) <$> (f x) <*> (g x)

-- A simpler solution for new comers would be to pattern match
addP :: (a -> Maybe Int) -> (a -> Maybe Int) -> a -> (Maybe Int)
addP f g x = 
    case (f x, g x) of
        (Just a, Just b) -> Just (a + b)
        _ -> Nothing


-- EXAMPLE 2
-- Original code
getNearestValuesBlind
    :: IntMap Double  -- ^ Map from positions to values
    -> Int            -- ^ Current position
    -> Double
getNearestValuesBlind vals pos
    -- both positions are in Map: returns sum of them
    | IntMap.member (pos - 1) vals && IntMap.member (pos + 1) vals =
        fromJust (IntMap.lookup (pos - 1) vals) + fromJust (IntMap.lookup (pos + 1) vals)

    -- only left position is in Map
    | IntMap.member (pos - 1) vals =
        fromJust (IntMap.lookup (pos - 1) vals)

    -- only right position is in Map
    | IntMap.member (pos + 1) vals =
        fromJust (IntMap.lookup (pos + 1) vals)

    -- no neighbours in map
    | otherwise = 0.0

-- Solution with pattern matching
getNearestValues :: IntMap Double -> Int -> Double
getNearestValues vals pos =
    case (IntMap.lookup (pos - 1 ) vals, IntMap.lookup (pos + 1) vals) of
        (Just left, Just right) -> left + right
        (Just left, Nothing) -> left
        (Nothing, Just right) -> right
        (Nothing, Nothing) -> 0.0