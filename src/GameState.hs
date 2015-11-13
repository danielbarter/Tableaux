module GameState where

import ContentVector
import Control.Monad.State.Lazy
import Data.List (transpose)

  
-- generating the initial game state

type STBuilder a = State [Int] a 

stBuilderTake :: [Int] -> STBuilder [[Int]]
stBuilderTake [] = return []
stBuilderTake (n:ns) = do s <- get
                          let (r,s') = splitAt n s
                          put s'
                          rs <- stBuilderTake ns
                          return (r:rs)


initialGameState :: [Int] -> StandardTableaux
initialGameState l = evalState (stBuilderTake l) [1..]

finalGameState :: [Int] -> StandardTableaux 
finalGameState p = transpose $ initialGameState $ transposePartition p
