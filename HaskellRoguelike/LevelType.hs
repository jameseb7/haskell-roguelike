module HaskellRoguelike.LevelType where

    import Control.Monad.State
    import Control.Monad.Writer
    import Data.Array
    import Data.Map (Map)
    import qualified Data.Map as Map

    import HaskellRoguelike.State
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
          entityMap :: Map EntityID (Entity Level),
          nextActors :: [EntityID],
          prevActors :: [EntityID]
        }
           
    levelHeight :: Int
    levelHeight = 20
    
    levelWidth :: Int
    levelWidth = 80
              
    symbolAt :: Level -> (Int, Int) -> Symbol
    symbolAt l p = cellSymbol ((cells l) ! p) (entityMap l)

    getCell :: (Int,Int) -> RoguelikeM Level Cell
    getCell p = do l <- get
                   return ((cells l) ! p)

    setCell :: (Int,Int) -> Cell -> RoguelikeM Level ()
    setCell p c = do l <- get
                     put l{cells = (cells l)//[(p,c)]}

    tellUpdateCell :: (Int,Int) -> RoguelikeM Level ()
    tellUpdateCell p = do l <- get
                          tell [UpdateCell p (symbolAt l p)]

    tellDrawLevel :: RoguelikeM Level ()
    tellDrawLevel = do l <- get
                       xs <- return $ assocs (cells l)
                       ys <- return $ map (\(p,c) -> (p,symbolAt l p)) xs
                       tell [DrawLevel 
                             (array ((0,0), (levelWidth-1,levelHeight-1)) ys)]
                   
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
                    
                  
                                  