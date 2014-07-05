module HaskellRoguelike.LevelType where

    import Data.Array
    import Data.Map (Map)
    import qualified Data.Map as Map

    import HaskellRoguelike.Symbol
    import HaskellRoguelike.EntityType

    data Cell = 
        Cell { 
          baseSymbol :: Symbol,
          entities :: [EntityID]
        }
          
          
    data Level = 
        Level { 
          cells :: Array (Int, Int) Cell,
          entityMap :: Map EntityID (Entity Level)
        }
           
    levelHeight :: Int
    levelHeight = 20
    
    levelWidth :: Int
    levelWidth = 80
              
    symbolAt :: Level -> (Int, Int) -> Symbol
    symbolAt l p = cellSymbol ((cells l) ! p) (entityMap l)

    cellSymbol :: Cell -> Map EntityID (Entity Level) -> Symbol
    cellSymbol c m = 
        case entities c of {
                          [] -> baseSymbol c;
                          e:es -> entitySymbol ((Map.!) m e)
                        }

    isClear :: Cell -> Map EntityID (Entity Level) -> Bool
    isClear c m = 
        let hasLargeEntity = 
                foldl 
                (\b -> \eid -> b || ((entitySize $ (Map.!) m eid) == Large))
                False (entities c)
            in
              if hasLargeEntity then
                  False
              else
                  case baseSymbol c of
                    Blank -> True
                    Floor -> True
                    _ -> False
                    
                  
                                  