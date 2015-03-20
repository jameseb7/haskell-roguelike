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

    putCells :: [Cell] -> [(Int,Int)] -> Level -> Level
    putCells cs ps l = l{cells = (cells l)//(zip ps cs)}

    putCellRect :: Cell -> ((Int,Int),(Int,Int)) -> Level -> Level
    putCellRect c b = putCells (repeat c) (range b)

    putCell :: Cell -> (Int,Int) -> Level -> Level
    putCell c p = putCells [c] [p]

    putCellMulti :: Cell -> [(Int,Int)] -> Level -> Level
    putCellMulti c = putCells (repeat c)

    lookupEntity :: Level -> EntityID -> Maybe Entity
    lookupEntity l e = Map.lookup e (entities l)
 
    entitiesAtCell :: Level -> Cell -> [Entity]
    entitiesAtCell l c = catMaybes $ map (lookupEntity l) $ cellEntities c

    compareEntitiesBySize :: Level -> EntityID -> EntityID -> Ordering
    compareEntitiesBySize l = compare `on` lookupSize
        where lookupSize =  maybe minBound entitySize . lookupEntity l

    addEntityAt :: Entity -> (Int,Int) -> Level -> Level
    addEntityAt e p l = l{
                          cells = accum (addEntityCell l) (cells l) [(p, eid)],
                          entities = Map.insert eid e' $ entities l
                        }       
        where e' = e{position = Left p}
              eid = entityID e

    addEntityCell :: Level -> Cell -> EntityID -> Cell
    addEntityCell l c eid = c{cellEntities = insertEntity (cellEntities c)}
        where insertEntity = insertBy (compareEntitiesBySize l) eid
    
    addEntityRandomClearM :: Entity -> RLState Level ()
    addEntityRandomClearM e = 
        do l <- get
           let ps = filter (isClear l) $ indices (cells l)
           i <- getRandomR (0,length ps - 1)
           modify $ addEntityAt e (ps!!i)
         
    cellSymbol :: Level -> Cell -> Symbol
    cellSymbol l c
        | visible c  = Visible $ case symbols of
                                   []  -> Left (baseSymbol c)
                                   x:_ -> Right x
        | explored c = Explored (baseSymbol c)
        | otherwise  = Unexplored
        where symbols = catMaybes . map entitySymbol $ entitiesAtCell l c

    blocksLOS :: Level -> (Int,Int) -> Bool
    blocksLOS l p = case baseSymbol (cells l ! p) of
                      BlankTerrain -> False
                      Floor        -> False
                      Rock         -> True
                      HWall        -> True
                      VWall        -> True

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

    updateCells :: (Cell -> Cell) -> Level -> Level
    updateCells f l = l{cells = fmap f (cells l)}

    displayLevel :: Level -> IO ()
    displayLevel l = printSymbolArray $ fmap (cellSymbol l) $ cells l

    testLevel :: Rand StdGen Level
    testLevel = do let l0 = uniformLevel (cell Floor)
                   let l1 = putCellRect (cell VWall) levelLeftBorder l0
                   let l2 = putCellRect (cell VWall) levelRightBorder l1
                   let l3 = putCellRect (cell HWall) levelTopBorder l2
                   let l4 = putCellRect (cell HWall) levelBottomBorder l3
                   xs <- getRandomRs (levelXMin, levelXMax)
                   ys <- getRandomRs (levelYMin, levelYMax)
                   let ps = take 10 $ zip xs ys
                   return $ putCellMulti (cell Rock) ps l4

    testLevelWithEntity :: RLState Level ()
    testLevelWithEntity = do (lift . lift) testLevel >>= put
                             lift testEntity >>= addEntityRandomClearM
