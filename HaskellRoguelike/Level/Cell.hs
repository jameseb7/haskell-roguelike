module HaskellRoguelike.Level.Cell 
    (Cell, baseSymbol, visible, explored, entities, 
     blankCell, cell, 
     blocksLOS, cellSymbol, isClearFor, 
     insertEntity, removeEntity) 
where
        
  import Data.List
  import Data.Ord
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
      
  import HaskellRoguelike.Entity.Type
  import HaskellRoguelike.Symbol

  data Cell = Cell { 
        baseSymbol :: TerrainSymbol,
        visible :: Bool,
        explored :: Bool,
        entities :: Map EntityID Entity,
        largeEntities :: [EntityID]
      }
              deriving (Eq, Show, Read)

  blankCell :: Cell
  blankCell = Cell BlankTerrain False False Map.empty [] 

  cell :: TerrainSymbol -> Cell
  cell s = blankCell{baseSymbol = s}
               
  cellSymbol :: Cell -> Symbol
  cellSymbol c
      | visible c  = 
          Visible $ 
          case largeEntities c of
            []  -> if Map.null (entities c)
                   then Left  $ baseSymbol c
                   else Right $ entitySymbol $ snd $ Map.findMin $ entities c
            e:_ -> Right $ entitySymbol $ entities c Map.! e
      | explored c = Explored $ baseSymbol c
      | otherwise  = Unexplored

  insertEntity :: Entity -> Cell -> Cell
  insertEntity e c = c{
                       entities = Map.insert (entityID e) e (entities c),
                       largeEntities = if entitySize e >= Large
                                      then entityID e : largeEntities c
                                      else largeEntities c
                     }

  removeEntity :: Entity -> Cell -> Cell
  removeEntity e c = c{
                       entities = Map.delete (entityID e) (entities c),
                       largeEntities = delete (entityID e) (largeEntities c)
                     }

  isClearFor :: Entity -> Cell -> Bool
  isClearFor e c = not isOccupied && 
                   case baseSymbol c of
                     BlankTerrain -> True
                     Floor        -> True
                     _            -> False
      where isOccupied = not $ null $ largeEntities c

  blocksLOS :: Cell -> Bool
  blocksLOS c = case baseSymbol c of
                  BlankTerrain -> True
                  Floor        -> True
                  _            -> False
    
    
