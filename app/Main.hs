module Main where

import System.IO
import ContentVector (isDecreasing)
import GameState
import Control.Monad.State.Lazy
import Text.Read (readMaybe)


parsePartition :: IO [Int]
parsePartition = do putStr "choose a weakly decreasing sequence like [3,3,2,1] : "
                    text <- getLine
                    let p = (readMaybe text) :: Maybe [Int]
                    case p of 
                      Just l  -> if isDecreasing l then putStrLn "the game begins!" >> return l
                                                   else putStrLn "your sequence was not weakly decreasing" >> parsePartition
                      Nothing -> putStrLn "sorry, I didn't catch that" >> parsePartition 

parseTransformation :: IO Int
parseTransformation = do putStr "transformation : "
                         text <- getLine
                         let x = (readMaybe text) :: Maybe Int
                         case x of Just m  -> return m
                                   Nothing -> putStrLn "sorry, I didn't catch that" >> parseTransformation

-- need to turn of line buffering for executable
main :: IO ()
main = do hSetBuffering stdin NoBuffering 
          hSetBuffering stdout NoBuffering
          p <- parsePartition
          enterGame p

enterGame :: [Int] -> IO ()
enterGame p = do state <- execStateT (gameOver) (initialState p)
                 putStrLn $ show state
                 playGame state
                 return ()

playGame :: GameState -> IO ()
playGame state = do let GameState e t f n b = state
                    if b then return ()
                         else do x <- parseTransformation
                                 newState <- execStateT (transform x >> gameOver) state
                                 putStrLn $ show newState
                                 playGame newState
                         
