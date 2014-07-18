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
          visible :: Bool,
          entities :: [EntityID]
        }
              deriving (Eq,Show)
          
    data Level = 
        Level {
          cells :: Array (Int, Int) Cell,
          entityMap :: Map EntityID (Entity Level),
          nextActors :: [EntityID],
          prevActors :: [EntityID],
          playerID :: Maybe EntityID
        }
           
    levelHeight :: Int
    levelHeight = 20
    
    levelWidth :: Int
    levelWidth = 80

    xMax = levelWidth - 1
    yMax = levelHeight - 1

    blankLevel = let c = Cell Blank True [] 
                 in Level {
                          cells = array ((0,0), (xMax,yMax)) 
                                  [(p,c) | p <- range ((0,0), (xMax,yMax))],
                          entityMap = Map.empty,
                          nextActors = [],
                          prevActors = [],
                          playerID = Nothing
                        }
              
    symbolAt :: Level -> (Int, Int) -> Symbol
    symbolAt l p = let c = (cells l) ! p
                   in if visible c then
                          cellSymbol c (entityMap l)
                      else
                          Blank

    getCell :: (Int,Int) -> RoguelikeM Level Cell
    getCell p = do l <- get
                   return ((cells l) ! p)

    setCell :: (Int,Int) -> Cell -> RoguelikeM Level ()
    setCell p c = do l <- get
                     put l{cells = (cells l)//[(p,c)]}

    tellUpdateCell :: (Int,Int) -> RoguelikeM Level ()
    tellUpdateCell p = 
        do l <- get
           case playerID l of
             Just pid -> 
                 do playerPos <- return (position $ (Map.!) (entityMap l) (pid))
                    los <- hasLOS playerPos p
                    case los of
                      True -> tell [UpdateCell p (symbolAt l p)]
                      False -> return ()

    tellDrawLevel :: RoguelikeM Level ()
    tellDrawLevel = 
        do l <- get
           case playerID l of
             Just pid -> 
                 do doFOV (position $ (Map.!) (entityMap l) (pid))
                    xs <- return $ assocs (cells l)
                    ys <- return $ map (\(p,c) -> (p,symbolAt l p)) xs
                    tell [DrawLevel 
                          (array ((0,0), (levelWidth-1,levelHeight-1)) ys)]
             Nothing -> return ()
                       
                   
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

    blocksLOS :: Cell -> Bool
    blocksLOS c = case baseSymbol c of
                    Blank -> True
                    Floor -> True
                    _ -> False
                    
    bresenhamLine :: (Int,Int) -> (Int,Int) -> (Cell -> (Bool,Cell)) -> RoguelikeM Level Bool
    bresenhamLine (x0,y0) (x1,y1) f = 
        let xBLLoop [] = return True
            xBLLoop (x:xs) = let y0' = fromIntegral y0 :: Double
                                 y1' = fromIntegral y1 :: Double
                                 x0' = fromIntegral x0 :: Double
                                 x1' = fromIntegral x1 :: Double
                                 x' = fromIntegral x :: Double
                                 y = round $ ((y1'-y0')/(x1'-x0'))*(x'-x0') + y0'
                                   :: Int
                             in do c <- getCell (x,y)
                                   (b,c') <- return $ f c
                                   setCell (x,y) c'
                                   case b of
                                     True -> xBLLoop xs
                                     False -> return False
            yBLLoop [] = return True
            yBLLoop (y:ys) = let y0' = fromIntegral y0 :: Double
                                 y1' = fromIntegral y1 :: Double
                                 x0' = fromIntegral x0 :: Double
                                 x1' = fromIntegral x1 :: Double
                                 y' = fromIntegral y :: Double
                                 x = round $ ((x1'-x0')/(y1'-y0'))*(y'-y0') + x0'
                                  :: Int
                             in do c <- getCell (x,y)
                                   (b,c') <- return $ f c
                                   setCell (x,y) c'
                                   case b of
                                     True -> yBLLoop ys
                                     False -> return False
        in
          if abs (x1 - x0) < abs (y1 - y0) then
              if y0 > y1 then
                  yBLLoop [y0,y0-1..y1]
              else 
                  yBLLoop [y0..y1]
          else
              if x0 > x1 then
                  xBLLoop [x0,x0-1..x1]
              else
                  xBLLoop [x0..x1]
          
    hasLOS :: (Int,Int) -> (Int,Int) -> RoguelikeM Level Bool
    hasLOS p0 p1 = bresenhamLine p0 p1 (\c -> (blocksLOS c, c))
      
    doFOV :: (Int,Int) -> RoguelikeM Level ()
    doFOV p0 = 
        let bl = (\p1 -> 
                  bresenhamLine p0 p1 (\c -> (blocksLOS c, c{visible=True})))
        in
          do 
            forM_ (range ((0,0), (xMax,yMax))) (\p -> getCell p >>= (\c -> setCell p c{visible=False}))
            forM_ (range ((0,0), (xMax,0))) bl
            forM_ (range ((0,0), (0,yMax))) bl
            forM_ (range ((0,yMax), (xMax,yMax))) bl
            forM_ (range ((xMax,0), (xMax,yMax))) bl