module HaskellRoguelike.EntityType where
    
    import HaskellRoguelike.Symbol
    import HaskellRoguelike.State
    import HaskellRoguelike.Action

    import Control.Monad.Trans
    import Control.Monad.State

    type EntityID = Int

    data EntitySize = Small | Large
                      deriving (Eq, Ord, Show, Enum, Bounded)

    data Entity c = Entity {
          entityID :: EntityID,
          position :: (Int,Int),
          entitySymbol :: Symbol,
          entitySize :: EntitySize,
          getAction :: Maybe (c -> RoguelikeM (Entity c) Action)
        }

    getEntityID :: RoguelikeM s EntityID
    getEntityID = lift $ state 
                  (\s -> 
                       let eid = nextEntityID s in
                       (fromIntegral eid, s{nextEntityID = eid + 1}))