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
    blankEntity = Entity 0 (0,0) Blank Small Nothing

    getPlayerAction :: Level -> RoguelikeM (Entity Level) Action
    getPlayerAction l = 
        do
          s <- lift get
          a <- return $ playerAction s
          lift (put s{playerAction = PlayerAction})
          return a

    makePlayer :: RoguelikeM s (Entity Level)
    makePlayer = 
        do 
          eid <- getEntityID
          return (Entity eid (0,0) Player Large (Just getPlayerAction))
          
                       