module ContentVector (ContentVector,
                      isContentVector,
                      StandardTableaux,
                      isStandardTableaux,
                      printStandardTableaux,
                      standardToContent,
                      contentToStandard) where

import Data.List (transpose)
import Text.PrettyPrint
import Control.Monad (join)
import Data.List (sort, sortBy)
import GHC.Exts (sortWith, groupWith)

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
                       && sort (join t) == [1..(length $ join t)]                        


-- safe maximum
maximum' :: [Int] -> Int
maximum' l = case l of [] -> 0
                       l  -> maximum l

-- when printing tableaux, you sometimes need to pad the strings
pad :: [[String]] -> [[String]]
pad t = (fmap . fmap) p t
    where m = maximum' $ maximum' <$> (fmap . fmap) length t
          p :: String -> String
          p s = take m (s ++ (repeat ' '))


printStandardTableaux :: StandardTableaux -> Doc
printStandardTableaux t = vcat $ hsep <$> t'
    where t' = (fmap . fmap) text $ pad $ (fmap . fmap) show t 

matrixZip :: [[a]] -> [[b]] -> [[(a,b)]]
matrixZip x y = (uncurry zip) <$> zip x y

contents :: [[Int]]
contents = [ (subtract j) <$> [0..] | j <- [0..] ] 
    where subtract j x = x - j

standardToContent :: StandardTableaux -> ContentVector
standardToContent t = snd <$> (sortWith fst $ join $ matrixZip t contents)

-- there is probably a better way to convert content vectors to standard tableaux
contentToStandard :: ContentVector -> StandardTableaux
contentToStandard c = foldl addDiagonal [] $ f <$> sortWith fst <$> (groupWith snd $ zip [1..] c)
    where f :: [(a,Int)] -> ([a],Int)
          f l = (fst <$> l, snd $ head l)
          addDiagonal :: [[a]] -> ([a],Int) -> [[a]]
          addDiagonal m (x:xs,n)
              | n <= 0 = [x] : (  uncurry (++) <$> zip m ((fmap return xs) ++ (repeat []))  )
              | n > 0  = uncurry (++) <$> zip m ( (fmap return (x:xs)) ++ (repeat []) )
