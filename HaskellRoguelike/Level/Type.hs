module HaskellRoguelike.Level.Type where

    data EntityMetadata = EntityMetadata {location :: Either (Int,Int) EntityID}

    data Level = 
        Level {
          cells :: Array (Int, Int) Cell,
          nextActors :: [EntityID],
          prevActors :: [EntityID],
          entityMap = Map EntityID EntityMetadata,
          playerID :: Maybe EntityID
        }

    instance EntityContext Level where
        getCell l (x,y) = if (x < x1) || (y < y1) || (x > x2) || (y > y2) 
                          then Nothing
                          else Just (cells l ! (x,y))
            where ((x1,y1),(x2,y2)) = bounds (cells l)
        setCell l p c = l{cells = cells l//[(p,c)]}
        contextSize _ = (levelWidth,levelHeight)

    levelHeight :: Int
    levelHeight = 20
    
    levelWidth :: Int
    levelWidth = 80

    xMax = levelWidth - 1
    yMax = levelHeight - 1

    blankLevel = Level {
                   cells = array ((0,0), (xMax,yMax)) 
                           [(p,blankCell) | p <- range ((0,0), (xMax,yMax))],
                   entityMap = Map.empty,
                   nextActors = [],
                   prevActors = [],
                   playerID = Nothing
                 }

    symbolAt :: Level -> (Int, Int) -> Symbol
    symbolAt l p = cellSymbol $ cells l ! p
