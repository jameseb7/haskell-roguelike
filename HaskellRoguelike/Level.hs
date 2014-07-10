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
    import Control.Monad.Writer

    import HaskellRoguelike.Action
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
          if isClear c (entityMap l) then 
              do 
                put l{
                   cells = (cells l)//[(p, c{entities = eid:(entities c)})],
                   entityMap = Map.insert eid e' (entityMap l)
                 } 
                l' <- get
                tell [UpdateCell p (symbolAt l' p)]
                return True
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

    runTurn :: RoguelikeM Level Action
    runTurn = do
      l <- get
      na <- return $ nextActors l
      case na of
        [] -> do 
          put l{
                nextActors = reverse (prevActors l),
                prevActors = []
              }
          return None
        eid:eids -> do
                actorLookup <- return $ Map.lookup eid (entityMap l)
                case actorLookup of 
                  Nothing -> put l{nextActors = eids} >> runTurn
                  Just actor -> case getAction actor of
                                  Nothing -> put l{nextActors = eids} >> runTurn
                                  Just ga -> do 
                                      (action, actor') <- lift $ runStateT (ga l) actor
                                      put l{entityMap = Map.insert eid actor' (entityMap l)}
                                      actionDone <- handleAction actor' action
                                      if actionDone then
                                          state (\l -> ((),l{
                                                          nextActors = eids,
                                                          prevActors = 
                                                              eid:(prevActors l)
                                                        }))
                                             >> runTurn
                                      else
                                          return action

    handleAction :: (Entity Level) -> Action -> RoguelikeM Level Bool
    handleAction e a = case a of
                         None -> return True
                         PlayerAction -> return False
                         Move dir -> moveEntity e dir

    moveEntity :: (Entity Level) -> Direction -> RoguelikeM Level Bool
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
                               if success then 
                                   do
                                     removeEntityFromCell e (x,y)
                                     l' <- get
                                     tell [UpdateCell (x,y) (symbolAt l' (x,y))]
                               else 
                                   return ()
                               if (dz /= 0) || (dw /= 0) then 
                                   return False
                               else
                                   return True

    removeEntityFromCell :: (Entity Level) -> (Int,Int) -> RoguelikeM Level ()
    removeEntityFromCell e p = 
        state (\l -> 
                   let eid = entityID e
                       c = (cells l) ! p
                       es = entities c
                       cells' = 
                           (cells l)//[(p,c{entities = filter ((/=) eid) es})]
                   in
                     ((), l{cells = cells'}))
                                  
              
