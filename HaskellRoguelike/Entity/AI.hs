module HaskellRoguelike.Entity.AI (getAction) where
    import Control.Monad
    import Control.Monad.State

    import HaskellRoguelike.State
    import HaskellRoguelike.Entity.Type
    import HaskellRoguelike.Entity.Context

    getAction :: AI -> Level -> RoguelikeM Entity Action
    getAction NoAI     l = return None
    getAction PlayerAI l = getPlayerAction l
                                  
    getPlayerAction :: Level -> RoguelikeM Entity Action
    getPlayerAction l = do s <- lift get
                           let a = playerAction s
                           lift $ put s{playerAction = PlayerAction}
                           return a
