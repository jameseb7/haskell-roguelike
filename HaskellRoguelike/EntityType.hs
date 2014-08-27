module HaskellRoguelike.EntityType where
    
    import HaskellRoguelike.Symbol
    import HaskellRoguelike.State
    import HaskellRoguelike.Action

    import Control.Monad.Trans
    import Control.Monad.State

    type EntityID = Int

    data EntitySize = Small | Large
                      deriving (Eq, Ord, Show, Enum, Bounded)

    data AI = NoAI | PlayerAI
        deriving (Eq, Show, Enum)

    data Entity c = Entity {
          entityID :: EntityID,
          position :: (Int,Int),
          entitySymbol :: EntitySymbol,
          entitySize :: EntitySize,
          ai :: AI
        }

    getEntityID :: RoguelikeM s EntityID
    getEntityID = lift $ state 
                  (\s -> 
                       let eid = nextEntityID s in
                       (fromIntegral eid, s{nextEntityID = eid + 1}))