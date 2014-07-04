module HaskellRoguelike.EntityType where
    
    import HaskellRoguelike.Symbol
    import HaskellRoguelike.State
    import HaskellRoguelike.Action

    import Control.Monad.Trans
    import Control.Monad.State

    type EntityID = Int

    data Entity c = Entity {
          entityID :: EntityID,
          position :: (Int,Int),
          entitySymbol :: Symbol,
          getAction :: Maybe (c -> RoguelikeM (Entity c) Action)
        }

    getEntityID :: RoguelikeM s EntityID
    getEntityID = lift $ state 
                  (\s -> 
                       let eid = nextEntityID s in
                       (fromIntegral eid, s{nextEntityID = eid + 1}))