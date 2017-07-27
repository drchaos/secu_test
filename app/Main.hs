import Prelude
import Data.Vector (Vector(..), fromList, (!?))
import Data.List
import Data.Maybe  (isJust)

calculateJumps :: [Int] -> Maybe Int
calculateJumps [] = Nothing
calculateJumps l
      | hasCycle indexes = Nothing
      | otherwise        = Just $ length indexes
  where
    indexes    = generateIndexes $ fromList l
    hasCycle l = isJust $ find (uncurry (==)) $ zip l $ every 2 l
    every n xs = case drop (n-1) xs of
                  (y:ys) -> y : every n ys
                  [] -> []

generateIndexes :: Vector Int -> [Int]
generateIndexes vec = unfoldr gen (vec,0)
  where gen (v,i) = case v !? i of
                      Just i' -> Just (i+i',(v,i+i'))
                      Nothing -> Nothing 


main :: IO ()
main =  do
    print $ calculateJumps [2,3,-1,1,3]
    print $ calculateJumps [1,1,-1,1]
    print $ calculateJumps $ replicate 100000 1


