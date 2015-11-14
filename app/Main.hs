module Main where

import ContentVector (isDecreasing)
import GameState
import Control.Monad.State.Lazy

main :: IO ()
main = do putStr "choose a weakly decreasing sequence like [3,3,2,1] : "
          text <- getLine
          let p = (read text) :: [Int]
          if isDecreasing p then do putStrLn $ "the game begins" 
                                    enterGame p
                            else putStrLn "your sequence was not weakly decreasing" >> return ()


enterGame :: [Int] -> IO ()
enterGame p = do state <- execStateT (gameOver) (initialState p)
                 putStrLn $ show state
                 playGame state
                 return ()

playGame :: GameState -> IO ()
playGame state = do let GameState e t f n b = state
                    if b then return ()
                         else do putStr "transformation : "
                                 text <- getLine
                                 let x = (read text) :: Int
                                 newState <- execStateT (transform x >> gameOver) state
                                 putStrLn $ show newState
                                 playGame newState
                         
