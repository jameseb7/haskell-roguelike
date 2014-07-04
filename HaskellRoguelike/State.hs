module HaskellRoguelike.State where

    import Data.Array

    import Control.Monad.State
    import Control.Monad.Random
    import Control.Monad.Writer

    import System.Random

    import HaskellRoguelike.Action
    import HaskellRoguelike.Symbol
    
    -- Datatype to contain global state
    data RLState = 
        RLState { 
          nextEntityID :: Int,
          playerAction :: Action
        }

    -- Datatype to indicate display actions in a safe, UI-independent way
    data RLDisplayAction =
        PutMessage String |
        UpdateCell Int Int Symbol |
        DrawLevel (Array (Int, Int) Symbol)

    -- Monad transformer stack to hold all the state required by the roguelike
    type RoguelikeM s a = 
        StateT s (StateT RLState (RandT StdGen (Writer [RLDisplayAction]))) a

    runRoguelikeM :: RoguelikeM s a -> s -> RLState -> StdGen 
                              -> (a, s, RLState, StdGen, [RLDisplayAction])
    runRoguelikeM f s rls g = (a, s', rls', g', w)
        where ((((a, s'), rls'), g'), w) = 
                  runWriter (runRandT (runStateT (runStateT f s) rls) g)