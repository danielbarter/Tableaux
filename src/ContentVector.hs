module ContentVector where

import Data.List (transpose)
import Text.PrettyPrint

type ContentVector = [Int]

-- there is probably a better way to test validity of a content vector
isContentVector :: ContentVector -> Bool
isContentVector [] = True
isContentVector [n] = if n == 0 then True else False
isContentVector list =    isContentVector ns  
                       && ((elem (n-1) ns) || (elem (n+1) ns)) 
                       && if (elem n ns) then (elem (n-1) interval) && (elem (n+1) interval) else True 
    where n  = last list
          ns = init list
          interval = takeWhile (/= n) (reverse ns)

type StandardTableaux = [[Int]]

-- test whether a list is a chain with respect to a total order
isChain :: (Ord a) => (a -> a -> Bool) -> [a] -> Bool
isChain _ [] = True
isChain _ [x] = True
isChain f l@(x:xs) = and [f p q | (p,q) <- zip l xs]


isIncreasing         :: (Ord a) => [a] -> Bool
isStrictlyIncreasing :: (Ord a) => [a] -> Bool
isDecreasing         :: (Ord a) => [a] -> Bool
isStrictlyDecreasing :: (Ord a) => [a] -> Bool

isIncreasing = isChain (<=)
isStrictlyIncreasing = isChain (<)
isDecreasing = isChain (\x y -> not (x < y))
isStrictlyDecreasing = isChain (>)


isStandardTableaux :: StandardTableaux -> Bool
isStandardTableaux [] = True
isStandardTableaux t =    (isDecreasing $ length <$> t)
                       && (and $ isStrictlyIncreasing <$> t)
                       && (and $ isStrictlyIncreasing <$> (transpose t))

