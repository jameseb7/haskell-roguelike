module HaskellRoguelike.Level where
    
    import Data.Array
    import Data.Function
    import Data.List
    import qualified Data.Map as Map
    import Data.Map (Map)
    import Data.Maybe

    import Control.Monad.Random
    import Control.Monad.State

    import HaskellRoguelike.Entity
    import HaskellRoguelike.State
    import HaskellRoguelike.Symbol

    adjustM :: (Monad m, Ord k) => (a -> m a) -> k -> Map k a -> m (Map k a)
    adjustM f k m = sequence (f <$> Map.lookup k m) >>= maybeInsertM k m
        where maybeInsertM k' m' = maybe (return m) (insertM k' m')
              insertM k' m' a    = return $ Map.insert k' a m'
        
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

    
    levelXMin :: Int
    levelXMin = 0

    levelYMin :: Int
    levelYMin = 0

    levelXMax :: Int
    levelXMax = 80

    levelYMax :: Int
    levelYMax = 20

    levelBounds :: ((Int,Int),(Int,Int))
    levelBounds = ((levelXMin,levelYMin), (levelXMax,levelYMax))
    
    levelTopBorder :: ((Int,Int),(Int,Int))
    levelTopBorder = ((levelXMin,levelYMin), (levelXMax,levelYMin))

    levelBottomBorder :: ((Int,Int),(Int,Int))
    levelBottomBorder = ((levelXMin,levelYMax), (levelXMax,levelYMax))

    levelLeftBorder :: ((Int,Int),(Int,Int))
    levelLeftBorder = ((levelXMin,levelYMin), (levelXMin,levelYMax))

    levelRightBorder :: ((Int,Int),(Int,Int))
    levelRightBorder = ((levelXMax,levelYMin), (levelXMax,levelYMax))

    inLevelBounds :: (Int,Int) -> Bool
    inLevelBounds (x,y) = (x >= levelXMin) || (x < levelXMax) ||
                        (y >= levelYMin) || (y < levelYMax)

    -- = Functions to construct levels

    -- A level filled uniformly with the specified cell
    uniformLevel :: Cell -> Level
    uniformLevel c = Level{
                       cells = listArray levelBounds (repeat c),
                       entities = Map.empty
                     }

    -- A blank cell
    blankCell :: Cell
    blankCell = Cell{
                  baseSymbol = BlankTerrain,
                  cellEntities = [],
                  explored = False,
                  visible = True
                }

    cell :: TerrainSymbol -> Cell
    cell s = blankCell{baseSymbol=s}

    -- A level filled with blank cells
    blankLevel :: Level
    blankLevel = uniformLevel blankCell

    -- Replaces the cells of a level with the specified cells 
    -- at the specified locations
    putCells :: [Cell] -> [(Int,Int)] -> Level -> Level
    putCells cs ps l = l{cells = (cells l)//(zip ps cs)}

    -- Replaces the cells of a levels with the specified cell type 
    -- within the rectangle having the specified corners
    putCellRect :: Cell -> ((Int,Int),(Int,Int)) -> Level -> Level
    putCellRect c b = putCells (repeat c) (range b)

    -- Replaces the cell at a given location with the given cell
    putCell :: Cell -> (Int,Int) -> Level -> Level
    putCell c p = putCells [c] [p]

    -- Replaces the cells at the given locations with copies of the given cell
    putCellMulti :: Cell -> [(Int,Int)] -> Level -> Level
    putCellMulti c = putCells (repeat c)

    -- = Obtaining information about a level

    -- Attempt to find an entity with the given ID within a level
    lookupEntity :: Level -> EntityID -> Maybe Entity
    lookupEntity l e = Map.lookup e (entities l)
 
    -- Find all the entities within a given cell 
    entitiesAtCell :: Level -> Cell -> [Entity]
    entitiesAtCell l = catMaybes . map (lookupEntity l) . cellEntities

    -- Compares two entities referenced by their IDs according to their size
    compareEntitiesBySize :: Level -> EntityID -> EntityID -> Ordering
    compareEntitiesBySize l = compare `on` lookupSize
        where lookupSize =  maybe minBound entitySize . lookupEntity l

    -- Returns the symbol of a particular cell in a level
    cellSymbol :: Level -> Cell -> Symbol
    cellSymbol l c
        | visible c  = Visible $ case symbols of
                                   []  -> Left (baseSymbol c)
                                   x:_ -> Right x
        | explored c = Explored (baseSymbol c)
        | otherwise  = Unexplored
        where symbols = catMaybes . map entitySymbol $ entitiesAtCell l c

    -- Returns whether or not a cell blocks line of sight
    blocksLOS :: Level -> (Int,Int) -> Bool
    blocksLOS l p = case baseSymbol (cells l ! p) of
                      BlankTerrain -> False
                      Floor        -> False
                      Rock         -> True
                      HWall        -> True
                      VWall        -> True

    -- Returns whether or not a cell is clear for moving into
    isClear :: Level -> (Int,Int) -> Bool
    isClear l p = case entitySizes of
                    []  -> isClearSymbol
                    x:_ -> if x >= Large then False else isClearSymbol
        where entitySizes = map entitySize $ entitiesAtCell l (cells l ! p)
              isClearSymbol = case baseSymbol (cells l ! p) of
                                BlankTerrain -> True
                                Floor        -> True
                                Rock         -> False
                                HWall        -> False
                                VWall        -> False

    -- Prints out a level for debug purposes
    displayLevel :: Level -> IO ()
    displayLevel l = printSymbolArray $ fmap (cellSymbol l) $ cells l

    -- = Mutating a level

    -- Exported functions in this section should either map to (RLState Level a)
    -- or (Level -> Level), allowing them to be used in monadic operations
    -- either directly or through use of modify
    
    -- Puts an entity at the given location in a level
    addEntityAt :: Entity -> (Int,Int) -> Level -> Level
    addEntityAt e p l = l{
                          cells = accum (addEntityCell l) (cells l) [(p, eid)],
                          entities = Map.insert eid e' $ entities l
                        }       
        where e' = e{position = Left p}
              eid = entityID e

    -- Helper function for addEntityAt, puts the given entity into the given cell
    addEntityCell :: Level -> Cell -> EntityID -> Cell
    addEntityCell l c eid = c{cellEntities = insertEntityCell (cellEntities c)}
        where insertEntityCell = insertBy (compareEntitiesBySize l) eid
    
    -- Puts an entity at a randomly chosen clear cell
    addEntityRandomClearM :: Entity -> RLState Level ()
    addEntityRandomClearM e = 
        do l <- get
           let ps = filter (isClear l) $ indices (cells l)
           i <- getRandomR (0,length ps - 1)
           modify $ addEntityAt e (ps!!i)

    -- moves the given entity to the given cell, does nothing if the entity is
    -- not in the level
    -- relocateEntity :: EntityID -> (Int,Int) -> RLState Level ()
    -- relocateEntity eid (x', y') = do
    --                             l <- get
    --                             let me = lookupEntity l eid
    --                             maybe (return ()) 

    -- applies a function to all cells in a level
    updateCells :: (Cell -> Cell) -> Level -> Level
    updateCells f l = l{cells = fmap f (cells l)}

    -- = Level constructors
                      
    -- Exported functions in this section should return something of type 
    -- EntityIDGenT (Rand StdGen) Level

    -- Makes a level with walls and pillars of rock for testing purposes
    testLevel :: EntityIDGenT (Rand StdGen) Level
    testLevel = do let l0 = uniformLevel (cell Floor)
                   let l1 = putCellRect (cell VWall) levelLeftBorder l0
                   let l2 = putCellRect (cell VWall) levelRightBorder l1
                   let l3 = putCellRect (cell HWall) levelTopBorder l2
                   let l4 = putCellRect (cell HWall) levelBottomBorder l3
                   xs <- getRandomRs (levelXMin+1, levelXMax-1)
                   ys <- getRandomRs (levelYMin+1, levelYMax-1)
                   let ps = take 10 $ zip xs ys
                   return $ putCellMulti (cell Rock) ps l4

    -- Makes a test level with an entity in for testing purposes
    testLevelWithEntity :: EntityIDGenT (Rand StdGen) Level
    testLevelWithEntity = do e <- testEntity
                             l <- testLevel
                             execStateT (addEntityRandomClearM e) l

    -- Apply a state function to an entity in a level
    promoteEntity :: EntityID -> RLState Entity a -> RLState Level (Maybe a)
    promoteEntity eid x = 
        do l <- get
           mea <- maybe (return Nothing) 
                  (fmap Just . lift . runStateT x)
                  (Map.lookup eid (entities l))
           case mea of
             Nothing -> return Nothing
             Just (a, e) -> 
                 do put l{entities = Map.insert eid e (entities l)}
                    return (Just a)
