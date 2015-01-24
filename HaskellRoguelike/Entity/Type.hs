module HaskellRoguelike.Entity.Type where
    
    import HaskellRoguelike.Symbol
    import HaskellRoguelike.State

    import Control.Monad.Trans
    import Control.Monad.State

    type EntityID = Int

    data EntitySize = Small | Large
                      deriving (Eq, Ord, Show, Read, Enum, Bounded)

    data AI = NoAI | PlayerAI
              deriving (Eq, Show, Read, Enum)

    data Entity = Entity {
          entityID :: EntityID,
          position :: (Int,Int),
          entitySymbol :: EntitySymbol,
          entitySize :: EntitySize,
          ai :: AI
        }
                  deriving (Eq, Show, Read)

    getEntityID :: RoguelikeM s EntityID
    getEntityID = lift $ state 
                  (\s -> let eid = nextEntityID s in
                         (fromIntegral eid, s{nextEntityID = eid + 1}))