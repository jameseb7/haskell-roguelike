module HaskellRoguelike.Entity 
    (
     module HaskellRoguelike.Entity,
     module HaskellRoguelike.EntityType
    ) where

    import Control.Monad.Trans
    import Control.Monad.State

    import HaskellRoguelike.Action
    import HaskellRoguelike.State
    import HaskellRoguelike.Symbol
    import HaskellRoguelike.EntityType
    import HaskellRoguelike.LevelType

    blankEntity :: Entity Level
    blankEntity = Entity 0 (0,0) BlankEntity Small NoAI

    getAction :: AI -> Level -> RoguelikeM (Entity Level) Action
    getAction a l = case a of
                      NoAI -> return None
                      PlayerAI -> getPlayerAction l

    getPlayerAction :: Level -> RoguelikeM (Entity Level) Action
    getPlayerAction _ = 
        do s <- lift get
           let a = playerAction s
           lift (put s{playerAction = PlayerAction})
           return a

    makePlayer :: RoguelikeM s (Entity Level)
    makePlayer = 
        do 
          eid <- getEntityID
          return (Entity eid (0,0) Player Large PlayerAI)
          
                       
