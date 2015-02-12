module HaskellRoguelike.Entity where
    
    import HaskellRoguelike.Symbol
    import HaskellRoguelike.State

    data EntitySize = Small | Large
                      deriving (Eq, Ord, Show, Enum, Bounded)

    data AI = PlayerAI | PassiveAI | RandomAI
        deriving (Eq, Show)

    data Entity = Entity {
          entityID :: EntityID,
          position :: Either (Int,Int) EntityID,
          entitySymbol :: EntitySymbol,
          entitySize :: EntitySize,
          ai :: Maybe AI
        }

    blankEntity :: (Monad m) => EntityIDGenT m Entity
    blankEntity = do eid <- newEntityID 
                     return Entity{
                                  entityID = eid,
                                  position = Left (0,0),
                                  entitySymbol = BlankEntity,
                                  entitySize = Small,
                                  ai = Nothing
                                }
