{-# LANGUAGE DoAndIfThenElse #-}

module HaskellRoguelike.Level 
    (
     module HaskellRoguelike.LevelType,
     module HaskellRoguelike.Level
    ) where

    import Data.Array
    import qualified Data.Map as Map
    import Control.Monad.Random
    import Control.Monad.State

    import HaskellRoguelike.Action
    import HaskellRoguelike.Symbol
    import HaskellRoguelike.State
    import HaskellRoguelike.Entity
    import HaskellRoguelike.LevelType

    uniformLevel :: Cell -> Level
    uniformLevel c = 
        let levelArray = array ((0,0), (xMax,yMax)) 
                         [ (p, c) | p <- range ((0,0), (xMax,yMax)) ]
        in blankLevel{cells = levelArray}

    setCells :: Cell -> [(Int,Int)] -> RoguelikeM Level ()
    setCells c ps = state (\l -> ((), l{cells = cells l//[(p,c) | p <- ps]}))

    wallLevel :: RoguelikeM Level ()
    wallLevel = do setCells blankCell{baseSymbol=VWall} (range ((0,    0),    (0,    yMax)))
                   setCells blankCell{baseSymbol=VWall} (range ((xMax, 0),    (xMax, yMax)))
                   setCells blankCell{baseSymbol=HWall} (range ((0,    0),    (xMax, 0)))
                   setCells blankCell{baseSymbol=HWall} (range ((0,    yMax), (xMax, yMax)))

    makeDefaultLevel :: RoguelikeM Level ()
    makeDefaultLevel = 
        do put $ uniformLevel (Cell Floor False False [])
           wallLevel
           xs <- getRandomRs (1, xMax-1)
           ys <- getRandomRs (1, yMax-1)
           setCells blankCell{baseSymbol=Rock} (zip (take 500 xs) (take 500 ys))

    makeMaze :: (Int,Int) -> RoguelikeM Level ()
    makeMaze p = do put $ uniformLevel (Cell Rock False False [])
                    wallLevel
                    setCellM p blankCell{baseSymbol=Floor}
                    makeMaze' (neighbours p []) [p]
        where makeMaze' [] _ = return ()
              makeMaze' xs ys = 
                  do (p1,p2) <- uniform xs
                     setCellM p1 blankCell{baseSymbol=Floor}
                     setCellM p2 blankCell{baseSymbol=Floor}
                     makeMaze' (neighbours p1 ys ++ 
                                           filter (\(a,_) -> a /= p1) xs) (p1:ys)
              neighbours (x,y) ys = 
                  filter (\(p',_) -> notElem p' ys) (p1++p2++p3++p4)
                  where p1 = if x <= 2 then [] else [((x-2,y),(x-1,y))]
                        p2 = if y <= 2 then [] else [((x,y-2),(x,y-1))]
                        p3 = if x >= xMax-2 then [] else [((x+2,y),(x+1,y))]
                        p4 = if y >= yMax-2 then [] else [((x,y+2),(x,y+1))]

                                


    putEntity :: Entity Level -> (Int,Int) -> RoguelikeM Level Bool
    putEntity e p = do l <- get
                       let c = cells l ! p
                       if isClear c (entityMap l) 
                       then putEntityForce e p >> return True
                       else return False
          
    putEntityForce :: Entity Level -> (Int,Int) -> RoguelikeM Level ()
    putEntityForce e p = 
        do l <- get
           let c = cells l ! p
           let eid = entityID e
           let e'  = e{position = p}
           put l{cells = cells l//[(p, c{entities = eid:entities c})],
                 entityMap = Map.insert eid e' (entityMap l)
                } 
           when (entitySymbol e == Player) $
                state (\l' -> ((),l'{playerID = Just eid}))
           tellUpdateCell p

    addActor :: Entity Level -> RoguelikeM Level ()
    addActor e = 
        state (\l ->
                   let pa = prevActors l
                       eid = entityID e
                   in
                     case ai e of
                       NoAI -> ((), l) 
                       _    -> ((), l{prevActors = eid:pa})
              )


    addEntity :: Entity Level -> (Int,Int) -> RoguelikeM Level Bool
    addEntity e p =
        do
          putDone <- putEntity e p
          if putDone then 
              do addActor e
                 return True
          else
              return False

    addEntityForce :: Entity Level -> (Int,Int) -> RoguelikeM Level ()
    addEntityForce e p = putEntityForce e p >> addActor e

    nextActor :: RoguelikeM Level ()
    nextActor = do l <- get
                   let pa = prevActors l
                   case nextActors l of
                     []   -> return ()
                     a:na -> put l{nextActors = na, prevActors = a:pa}
   
    dropActor :: RoguelikeM Level ()
    dropActor = get >>= (\ l -> put l{nextActors = tail $ nextActors l})

    currentActor :: RoguelikeM Level (Maybe (Entity Level))
    currentActor = do l <- get
                      case nextActors l of
                        []   -> return Nothing
                        a:_ -> case Map.lookup a (entityMap l) of
                                  Nothing -> dropActor >> currentActor
                                  Just a' -> return $ Just a'

    insertEntityToMap :: Entity Level -> RoguelikeM Level ()
    insertEntityToMap e = let eid = entityID e in
                          do l <- get
                             put l{entityMap = Map.insert eid e (entityMap l)}

    resetActors :: RoguelikeM Level ()
    resetActors = do l <- get
                     let pa = prevActors l
                     let na = nextActors l
                     put l{nextActors = reverse pa ++ na, prevActors = []}

    runTurn :: RoguelikeM Level Action
    runTurn = resetActors >> runTurn'

    runTurn' :: RoguelikeM Level Action
    runTurn' = 
        do level <- get 
           mActor <- currentActor
           case mActor of
             Nothing    -> return None
             Just actor -> do 
                         (action, actor') <- lift $ runStateT (getAction (ai actor) level) actor
                         insertEntityToMap actor'
                         actionDone <- handleAction actor' action
                         if actionDone 
                         then nextActor >> runTurn 
                         else return action

    handleAction :: Entity Level -> Action -> RoguelikeM Level Bool
    handleAction _ None         = return True
    handleAction _ PlayerAction = return False
    handleAction e (Move dir)   = moveEntity e dir

    moveEntity :: Entity Level -> Direction -> RoguelikeM Level Bool
    moveEntity e dir = let (dx,dy,dz,dw) = toOffset dir
                           eid = entityID e
                           (x,y) = position e
                           x' = x+dx
                           y' = y+dy
                       in
                         if (y' < 0) || (y' >= levelHeight) || 
                                (x' < 0) || (x' >= levelWidth) then
                             return False
                         else
                             do success <- putEntity e (x',y')
                                when success $
                                   do removeEntityFromCell e (x,y)
                                      l <- get
                                      case playerID l of
                                        Just pid -> if eid == pid then 
                                                        tellDrawLevel
                                                    else
                                                         tellUpdateCell (x,y)
                                        Nothing -> tellUpdateCell (x,y)
                                return $ not ((dz /= 0) || (dw /= 0))
 
    removeEntityFromCell :: Entity Level -> (Int,Int) -> RoguelikeM Level ()
    removeEntityFromCell e p = 
        state (\l -> 
                   let eid = entityID e
                       c = cells l ! p
                       es = entities c
                       cells' = 
                           cells l//[(p,c{entities = filter (eid /=) es})]
                   in
                     ((), l{cells = cells'}))
                                  
              
