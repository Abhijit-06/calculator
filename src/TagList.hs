module TagList where

import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty (..))
import Tag (Tag, new)

newtype TagList = TagList (NonEmpty Tag) deriving (Show)

newList :: [String] -> Maybe TagList
newList [] = Nothing
newList (t:ts) = TagList <$> liftA2 (:|)  (new t) (traverse new ts)