{-# LANGUAGE DoAndIfThenElse #-}

module HaskellRoguelike.Level 
    (
     module HaskellRoguelike.LevelType,
     module HaskellRoguelike.Level
    ) where

    import Data.Array
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Data.List
    import Control.Monad.Random
    import Control.Monad.State
    import Control.Monad.Writer

    import HaskellRoguelike.Action
    import HaskellRoguelike.Symbol
    import HaskellRoguelike.State
    import HaskellRoguelike.Entity
    import HaskellRoguelike.EntityType
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
              makeMaze' xs ys = do (p,p') <- uniform xs
                                   setCellM p blankCell{baseSymbol=Floor}
                                   setCellM p' blankCell{baseSymbol=Floor}
                                   makeMaze' (neighbours p ys ++ filter (\(a,b) -> a /= p) xs) (p:ys)
              neighbours (x,y) ys = let p1 = if x <= 2 then [] else [((x-2,y),(x-1,y))]
                                        p2 = if y <= 2 then [] else [((x,y-2),(x,y-1))]
                                        p3 = if x >= xMax-2 then [] else [((x+2,y),(x+1,y))]
                                        p4 = if y >= yMax-2 then [] else [((x,y+2),(x,y+1))]
                                    in filter (\(p,p') -> notElem p ys) (p1++p2++p3++p4)
                                


    putEntity :: Entity Level -> (Int,Int) -> RoguelikeM Level Bool
    putEntity e p = 
        let eid = entityID e
            e'  = e{position = p}
        in do
          l <- get
          let c = cells l ! p
          if isClear c (entityMap l) then 
              putEntityForce e p >> return True
          else
              return False
          
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
                state (\l -> ((),l{playerID = Just eid}))
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

    runTurn :: RoguelikeM Level Action
    runTurn = 
        do l <- get
           let na = nextActors l
           case na of
             [] -> do 
               put l{
                     nextActors = reverse (prevActors l),
                     prevActors = []
                   }
               return None
             eid:eids -> do
                     let actorLookup = Map.lookup eid (entityMap l)
                     case actorLookup of 
                       Nothing -> put l{nextActors = eids} >> runTurn
                       Just actor -> 
                           case ai actor of
                             NoAI -> put l{nextActors = eids} >> runTurn
                             actorAI -> do 
                                        (action, actor') <- lift $ runStateT (getAction actorAI l) actor
                                        put l{entityMap = Map.insert eid actor' (entityMap l)}
                                        actionDone <- handleAction actor' action
                                        if actionDone then
                                            state (\l -> ((),l{
                                                            nextActors = eids,
                                                            prevActors = 
                                                                eid:prevActors l
                                                          }))
                                               >> runTurn
                                        else
                                            return action

    handleAction :: Entity Level -> Action -> RoguelikeM Level Bool
    handleAction e a = case a of
                         None -> return True
                         PlayerAction -> return False
                         Move dir -> moveEntity e dir

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
                             do
                               success <- putEntity e (x',y')
                               when success $
                                   do
                                     removeEntityFromCell e (x,y)
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
                                  
              
