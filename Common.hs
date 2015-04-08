module Common where

import Data.Maybe (mapMaybe)

-- | filter and map together
mapfilter :: (a -> Maybe b) -> [a] -> [b]
mapfilter = mapMaybe

-- | filter and map together
filtermap = mapfilter
