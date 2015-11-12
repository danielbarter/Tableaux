module ContentVector where

type ContentVector = [Int]

-- there is probably a better way to test validity
isValid :: ContentVector -> Bool
isValid [] = True
isValid [n] = if n == 0 then True else False
isValid list =    isValid ns  
               && ((elem (n-1) ns) || (elem (n+1) ns)) 
               && if (elem n ns) then (elem (n-1) interval) && (elem (n+1) interval) else True 
    where n  = last list
          ns = init list
          interval = takeWhile (/= n) (reverse ns)

type StandardTableaux = [[Int]]

isDecreasing :: (Ord a) => [a] -> 
