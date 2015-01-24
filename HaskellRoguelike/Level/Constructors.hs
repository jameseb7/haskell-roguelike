module HaskellRoguelike.Level.Constructors where
    
    import Control.Monad

    uniformLevel :: Cell -> Level
    uniformLevel c = blankLevel{cells = levelArray}
        where levelArray = array ((0,0), (xMax,yMax)) 
                           map (\ p -> (p,c)) $ range ((0,0), (xMax,yMax))
                    
    setCells :: Cell -> [(Int,Int)] -> RoguelikeM Level ()
    setCells c ps = mapM_ (\ p -> setCellM p c) ps

    wallLevel :: RoguelikeM Level ()
    wallLevel = do setCells (cell VWall) $ range ((0,    0   ), (0,    yMax))
                   setCells (cell VWall) $ range ((xMax, 0   ), (xMax, yMax))
                   setCells (cell HWall) $ range ((0,    0   ), (xMax, 0   ))
                   setCells (cell HWall) $ range ((0,    yMax), (xMax, yMax))

    makeDefaultLevel :: RoguelikeM Level ()
    makeDefaultLevel = do put $ uniformLevel (cell Floor)
                          wallLevel
                          xs <- getRandomRs (1, xMax-1)
                          ys <- getRandomRs (1, yMax-1)
                          setCells (cell Rock) (take 500 $ zip xs ys)

    makeMaze :: (Int,Int) -> RoguelikeM Level ()
    makeMaze p = do put $ uniformLevel (Cell Rock False False [])
                    wallLevel
                    setCellM p $ cell Floor
                    makeMaze' (neighbours p []) [p]
        where makeMaze' [] _ = return ()
              makeMaze' xs ys = 
                  do (p1,p2) <- uniform xs
                     setCellM p1 $ cell Floor
                     setCellM p2 $ cell Floor
                     makeMaze' (neighbours p1 ys ++ 
                                filter (\(a,_) -> a /= p1) xs) (p1:ys)
              neighbours (x,y) ys = 
                  filter (\(p',_) -> notElem p' ys) (p1++p2++p3++p4)
                  where p1 = if x <= 2 then [] else [((x-2,y),(x-1,y))]
                        p2 = if y <= 2 then [] else [((x,y-2),(x,y-1))]
                        p3 = if x >= xMax-2 then [] else [((x+2,y),(x+1,y))]
                        p4 = if y >= yMax-2 then [] else [((x,y+2),(x,y+1))]