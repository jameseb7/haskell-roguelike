{-# LANGUAGE DoAndIfThenElse #-}

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
          Level levelArray Map.empty [] []

    putEntity :: Entity Level -> (Int,Int) -> RoguelikeM Level Bool
    putEntity e p = 
        let eid = entityID e
            e'  = e{position = p}
            in do
              l <- get
              c <- return ((cells l) ! p)
              if isClear c (entityMap l) 
              then do {
                     put l{
                       cells = 
                           (cells l)//[(p, c{entities = eid:(entities c)})],
                       entityMap = Map.insert eid e' (entityMap l)
                     };
                     return True
                   }
              else
                  return False

    addActor :: Entity Level -> RoguelikeM Level ()
    addActor e = 
        state (\l ->
                   let pa = prevActors l
                       eid = entityID e
                       ga = getAction e
                   in
                     case ga of
                       Nothing -> ((), l) 
                       Just _  -> ((), l{prevActors = eid:pa})
              )


    addEntity :: Entity Level -> (Int,Int) -> RoguelikeM Level Bool
    addEntity e p =
        do
          putDone <- putEntity e p
          if putDone then 
              do
                addActor e
                return True
          else
              return False 