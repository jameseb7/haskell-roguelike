module HaskellRoguelike.Entity where
    
    import qualified Data.Set as Set
    import Data.Set (Set)

    import HaskellRoguelike.Symbol
    import HaskellRoguelike.State

    data EntitySize = Small | Large
                      deriving (Eq, Ord, Show, Enum, Bounded)

    data AI = PlayerAI | PassiveAI | RandomAI
              deriving (Eq, Show)

    data EntityProperty = Invisible
                          deriving (Eq, Ord, Show, Enum, Bounded)

    data Entity = Entity {
          entityID :: EntityID,
          position :: Either (Int,Int) EntityID,
          baseEntitySymbol :: EntitySymbol,
          entitySize :: EntitySize,
          entityProperties :: Set EntityProperty,
          ai :: Maybe AI
        }

    blankEntity :: (Monad m) => EntityIDGenT m Entity
    blankEntity = do eid <- newEntityID 
                     return Entity{
                                  entityID = eid,
                                  position = Left (0,0),
                                  baseEntitySymbol = BlankEntity,
                                  entitySize = Small,
                                  entityProperties = Set.empty,
                                  ai = Nothing
                                }

    hasProperty :: Entity -> EntityProperty -> Bool
    hasProperty e p = Set.member p $ entityProperties e

    entitySymbol :: Entity -> Maybe EntitySymbol
    entitySymbol e
        | e `hasProperty` Invisible = Nothing
        | otherwise = Just $ baseEntitySymbol e

    
