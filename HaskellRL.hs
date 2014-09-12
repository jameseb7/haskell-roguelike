import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer
import Data.Array

import UI.HSCurses.Curses

import HaskellRoguelike.Action
import HaskellRoguelike.Entity
import HaskellRoguelike.Level
import HaskellRoguelike.State
import HaskellRoguelike.Symbol

main :: IO ()
main = 
    do g <- getStdGen
       writeFile "HaskellRL.log" ""
       appendFile "HaskellRL.log" "setting up game...\n"
       let (l,_,rls,g',w) = runRoguelikeM setupGame () (RLState 0 None) g
       sa <- handleRLDisplayActions w blankSymbolArray
       _ <- initScr
       echo False
       cBreak True
       nl False
       drawSymbolArray sa
       refresh
       appendFile "HaskellRL.log" "game set up\n"
       runGame l rls g' sa
       endWin

blankSymbolArray :: Array (Int,Int) Symbol
blankSymbolArray = fmap (\ _ -> Unexplored) (array ((0,0), (xMax,yMax)) [])
                                                  
setupGame :: RoguelikeM () Level
setupGame = 
    do 
      tell [LogMessage "setupGame started\n"]
      p <- makePlayer
      x <- getRandomR (3, levelWidth-4)
      y <- getRandomR (3, levelHeight-4)
      let x' = if mod x 2 == 0 then x + 1 else x
      let y' = if mod y 2 == 0 then y + 1 else y
      tell [LogMessage "player made\n"]
      (_,l) <- lift $ runStateT (makeMaze (x',y')) blankLevel
      tell [LogMessage "level made\n"]
      (_,l') <- lift $ runStateT (addEntityForce p (x',y')) l
      (_,l'') <- lift $ runStateT tellDrawLevel l'
      return l''

runGame :: Level -> RLState -> StdGen -> Array (Int, Int) Symbol -> IO ()
runGame l rls g sa = 
    do (a,l',rls',g',w) <- return $ runRoguelikeM runTurn l rls g
       sa' <- handleRLDisplayActions w sa
       drawSymbolArray sa'
       move 0 0
       refresh
       case a of
         None -> runGame l' rls' g' sa'
         PlayerAction -> do a' <- readActionFromPlayer
                            case a' of
                              None -> return ()
                              _    -> runGame l' rls'{playerAction = a'} g' sa'
         _ -> error ("Invalid Action at toplevel: " ++ show a)

handleRLDisplayActions :: [RLDisplayAction] 
                       -> Array (Int, Int) Symbol 
                       -> IO (Array (Int, Int) Symbol)
handleRLDisplayActions [] sa   = return sa
handleRLDisplayActions (x:xs) sa = 
    case x of
      PutMessage str -> do drawSymbolArray sa
                           putMessage str
                           refresh
                           move 0 0
                           clrToEol
                           handleRLDisplayActions xs sa
      UpdateCell p s -> handleRLDisplayActions xs (sa//[(p,s)])
      DrawLevel sa'  -> handleRLDisplayActions xs sa'
      LogMessage str -> do appendFile "HaskellRL.log" str
                           handleRLDisplayActions xs sa

readActionFromPlayer :: IO Action
readActionFromPlayer = do c <- getCh
                          case c of
                            KeyChar '.' -> return $ Move Here
                            KeyChar '8' -> return $ Move North
                            KeyChar 'k' -> return $ Move North
                            KeyUp       -> return $ Move North
                            KeyChar '6' -> return $ Move East
                            KeyChar 'l' -> return $ Move East
                            KeyRight    -> return $ Move East
                            KeyChar '2' -> return $ Move South
                            KeyChar 'j' -> return $ Move South
                            KeyDown     -> return $ Move South
                            KeyChar '4' -> return $ Move West
                            KeyChar 'h' -> return $ Move West
                            KeyLeft     -> return $ Move West
                            KeyChar '9' -> return $ Move NorthEast
                            KeyChar 'u' -> return $ Move NorthEast
                            KeyChar '7' -> return $ Move NorthWest
                            KeyChar 'y' -> return $ Move NorthWest
                            KeyChar '3' -> return $ Move SouthEast
                            KeyChar 'n' -> return $ Move SouthEast
                            KeyChar '1' -> return $ Move SouthWest
                            KeyChar 'b' -> return $ Move SouthWest
                            KeyChar 'q' -> return None
                            _           -> readActionFromPlayer
                            
putMessage :: String -> IO ()
putMessage str = do move 0 0 >> clrToEol
                    mvWAddStr stdScr 0 0 str
                    refresh
                    void getCh
                       
charTerrainSymbol :: TerrainSymbol -> Char
charTerrainSymbol s  = case s of
                        BlankTerrain  -> ' '
                        Floor  -> '.'
                        Rock   -> '#'
                        HWall  -> '-'
                        VWall  -> '|'

charEntitySymbol :: EntitySymbol -> Char
charEntitySymbol s = case s of
                       BlankEntity -> ' '
                       Player -> '@'
  
drawSymbol :: (Int,Int) -> Symbol -> IO ()
drawSymbol (x,y) s = mvWAddStr stdScr (y+1) x [c]
    where c = case s of 
                Visible s' -> case s' of 
                                Left  terrainS -> charTerrainSymbol terrainS
                                Right entityS -> charEntitySymbol entityS
                Explored s' -> charTerrainSymbol s'
                Unexplored -> ' '
                                            

drawSymbolArray :: Array (Int, Int) Symbol -> IO ()
drawSymbolArray sa = forM_ xs (\x -> drawSymbol x (sa ! x))
    where xs = range ((0,0), (xMax,yMax))
