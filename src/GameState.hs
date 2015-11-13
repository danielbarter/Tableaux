module GameState where

import ContentVector
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except

data Error = NotAPartition
           | NotAValidTransformation

type GameState a = ExceptT Error (State StandardTableaux) a 
