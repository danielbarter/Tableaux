module GameState (Game,
                  GameState(..),
                  initialState,
                  transform,
                  gameOver) where

import ContentVector
import Text.PrettyPrint (render)
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


initialTableaux :: [Int] -> StandardTableaux
initialTableaux l = evalState (stBuilderTake l) [1..]

finalTableaux :: [Int] -> StandardTableaux 
finalTableaux p = transpose $ initialTableaux $ transposePartition p

-- the game state
data Error = NotValidTransformation
    deriving (Eq)

instance Show Error where
    show NotValidTransformation = "Not a valid transformation"

type GameOver = Bool

-- (error, current state, final state, allowable transformations, game over?)
data GameState = GameState (Maybe Error) StandardTableaux StandardTableaux Int GameOver
    deriving (Eq)

instance Show GameState where
    show gamestate = case gamestate of GameState e t f n True -> "Game Over!"
                                       GameState Nothing t f n b  -> render $ printStandardTableaux t
                                       GameState (Just e) t f n b -> (show e) ++ "\n" ++ (render $ printStandardTableaux t) 

-- game monad transformer
type Game m = StateT GameState m

initialState :: [Int] -> GameState
initialState p =  GameState Nothing (initialTableaux p) (finalTableaux p) (sum p) False

transform :: (Monad m) => Int -> Game m ()
transform x = do GameState e t f n b <- get
                 if (1 <= x ) && (x < n) 
                     then put (GameState Nothing (transformTableaux x t) f n b) >> return ()
                     else put (GameState (Just NotValidTransformation) t f n b) >> return ()

gameOver :: (Monad m) =>  Game m ()
gameOver = do GameState e t f n b <- get
              if (t == f) 
                  then put (GameState e t f n True) >> return ()
                  else put (GameState e t f n b)    >> return ()

