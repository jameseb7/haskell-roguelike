module HaskellRoguelike.Level where
    
    import Data.Array
    import Data.Function
    import Data.List
    import qualified Data.Map as Map
    import Data.Map (Map)
    import Data.Maybe

    import HaskellRoguelike.Entity
    import HaskellRoguelike.State
    import HaskellRoguelike.Symbol

    data Cell = Cell{
               baseSymbol :: TerrainSymbol,
               cellEntities :: [EntityID],
               explored :: Bool,
               visible :: Bool
             }

    data Level = Level{
               cells :: Array (Int,Int) Cell,
               entities :: Map EntityID Entity
             }

    levelXMax :: Int
    levelXMax = 80
    levelYMax :: Int
    levelYMax = 20

    blankCell :: Cell
    blankCell = Cell{
                  baseSymbol = BlankTerrain,
                  cellEntities = [],
                  explored = False,
                  visible = False
                }

    blankLevel :: Level
    blankLevel = Level{
                   cells = listArray ((0,0),(levelXMax,levelYMax)) blanks,
                   entities = Map.empty
                 }
        where blanks = repeat blankCell

    lookupEntity :: Level -> EntityID -> Maybe Entity
    lookupEntity l e = Map.lookup e (entities l)
 
    entitiesAtCell :: (Int,Int) -> Level -> [Entity]
    entitiesAtCell c l = catMaybes $ map (lookupEntity l) $ cellEntities cell
        where cell = cells l ! c

    compareEntitiesBySize :: Level -> EntityID -> EntityID -> Ordering
    compareEntitiesBySize l = compare `on` lookupSize
        where lookupSize =  maybe minBound entitySize . lookupEntity l

    addEntityAt :: Level -> (Int,Int) -> Entity -> Level
    addEntityAt l p e = l{
                          cells = accum (addEntityCell l) (cells l) [(p, eid)],
                          entities = Map.insert eid e' $ entities l
                        }       
        where e' = e{position = Left p}
              eid = entityID e

    addEntityCell :: Level -> Cell -> EntityID -> Cell
    addEntityCell l c eid = c{cellEntities = insertEntity (cellEntities c)}
        where insertEntity = insertBy (compareEntitiesBySize l) eid
             
    cellSymbol :: Level -> Cell -> Symbol
    cellSymbol l c
        | visible c  = Visible $ case entityLookups $ cellEntities c of
                                   []  -> Left (baseSymbol c)
                                   x:_ -> Right (entitySymbol x)
        | explored c = Explored (baseSymbol c)
        | otherwise  = Unexplored
        where entityLookups = catMaybes . map (lookupEntity l)

    printLevel :: Level -> IO ()
    printLevel l = printSymbolArray $ fmap (cellSymbol l) $ cells l
