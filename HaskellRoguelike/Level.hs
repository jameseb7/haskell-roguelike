module HaskellRoguelike.Level 
    (
     module HaskellRoguelike.LevelType,
     module HaskellRoguelike.Level
    ) where

    import Data.Array
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Control.Monad.State

    import HaskellRoguelike.Symbol
    import HaskellRoguelike.State
    import HaskellRoguelike.EntityType
    import HaskellRoguelike.LevelType

    makeDefaultLevel :: RoguelikeM s Level
    makeDefaultLevel = return defaultLevel

    defaultLevel = 
        let chooseCell (x,y)
                | x == 0 = Cell VWall []
                | x == levelWidth-1 = Cell VWall []
                | y == 0 = Cell HWall []
                | y == levelHeight-1 = Cell HWall []
                | otherwise = Cell Floor []
            levelArray = array ((0,0), (levelWidth-1,levelHeight-1)) 
                         [ (p, chooseCell p) | 
                           p <- range ((0,0), (levelWidth-1,levelHeight-1)) ]
        in
          Level levelArray Map.empty

    addEntity :: Entity Level -> (Int,Int) -> RoguelikeM Level Bool
    addEntity e p = 
        let eid = entityID e
            e'  = e{position = p}
            in do {
                 l <- get;
                 c <- return ((cells l) ! p);
                 put l{
                   cells = (cells l)//[(p, c{entities = eid:(entities c)})],
                   entityMap = Map.insert eid e' (entityMap l)
                 };
                 return True
               }
                  
      
    