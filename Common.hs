module Common where

import Data.Maybe (mapMaybe)

-- | filter and map together
mapfilter :: (a -> Maybe b) -> [a] -> [b]
mapfilter = mapMaybe

-- | filter and map together
filtermap = mapfilter

-- | Ordered combinations of sub lists
--   Should think of better name
--   Given [[a,b],[c],[d,e,f]] returns [[a,c,d],[a,c,e],[a,c,f],[b,c,d],[b,c,e],[b,c,f]]
combos :: [[a]] -> [[a]]
combos as =
  case as of
    [a] -> [ [x] | x <- a ] -- given [[a,b]] returns [[a],[b]]
    (a:b:rest) -> foldl fo (go a b) rest
  where
    -- Given [a,b] [1,2] returns [[a,1],[a,2],[b,1],[b,2]]
    go :: [a] -> [a] -> [[a]]
    go xs ys = [[x,y] | x<-xs, y<-ys]

    -- Given [[a,b],[c,d]] [1,2] returns [[a,b,1],[c,d,1],[a,b,2],[c,d,2]]
    fo :: [[a]] -> [a] -> [[a]]
    fo xss ys = [ xs++[y] | xs<-xss, y<-ys]
