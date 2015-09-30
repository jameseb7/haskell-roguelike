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
          ai :: Maybe AI,
          inventory :: Maybe (Set EntityID)
        }

    blankEntity :: (Monad m) => EntityIDGenT m Entity
    blankEntity = do eid <- newEntityID 
                     return Entity{
                                  entityID = eid,
                                  position = Left (0,0),
                                  baseEntitySymbol = BlankEntity,
                                  entitySize = Small,
                                  entityProperties = Set.empty,
                                  ai = Nothing,
                                  inventory = Nothing
                                }

    hasProperty :: Entity -> EntityProperty -> Bool
    hasProperty e p = Set.member p $ entityProperties e

    entitySymbol :: Entity -> Maybe EntitySymbol
    entitySymbol e
        | e `hasProperty` Invisible = Nothing
        | otherwise = Just $ baseEntitySymbol e

    setProperty :: EntityProperty -> Entity -> Entity
    setProperty p e = e{entityProperties = Set.insert p $ entityProperties e}

    clearProperty :: EntityProperty -> Entity -> Entity
    clearProperty p e = e{entityProperties = Set.delete p $ entityProperties e}

    setXYPosition :: (Int,Int) -> Entity -> Entity
    setXYPosition pos e = e{position = Left pos}

    -- This function fails if the container identifier is the identifier of the
    -- entity being updated
    setContainer :: EntityID -> Entity -> Entity
    setContainer c e
        | c /= entityID e = e{position = Right c}
        | otherwise = error "setContainer: can't insert an entity into itself"

    -- This function fails if the entity identifier being inserted is the same
    -- as the identifier of the entity being inserted into or if the entity
    -- being inserted into has no inventory
    insertEntity :: EntityID -> Entity -> Entity
    insertEntity eid e
        | eid /= entityID e = e{inventory = Set.insert eid <$> inventory e}
        | otherwise = error "insertEntity: can't insert an entity into itself"

    removeEntity :: EntityID -> Entity -> Entity
    removeEntity eid e = e{inventory = Set.delete eid <$> inventory e}

    testEntity :: (Monad m) => EntityIDGenT m Entity
    testEntity = do e <- blankEntity
                    return e{baseEntitySymbol = TestEntity1,
                             entitySize = Large}
