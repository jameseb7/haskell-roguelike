import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import System.Random
import Data.Array
import Data.Ix

import UI.HSCurses.Curses

import HaskellRoguelike.Action
import HaskellRoguelike.Entity
import HaskellRoguelike.Level
import HaskellRoguelike.State
import HaskellRoguelike.Symbol

main = 
    do initScr
       echo False
       cBreak True
       nl False
       g <- getStdGen
       (l,_,rls,g',w) <- return $ runRoguelikeM setupGame () (RLState 0 None) g
       mapM_ handleRLDisplayAction w
       refresh
       runGame l rls g'
       endWin
                                                  
setupGame :: RoguelikeM () Level
setupGame = 
    do
      p <- makePlayer
      (_,l) <- lift $ runStateT makeDefaultLevel blankLevel
      x <- getRandomR (1, levelWidth-2)
      y <- getRandomR (1, levelHeight-2)
      (_,l') <- lift $ runStateT (addEntity p (x,y)) l
      ((),l'') <- lift $ runStateT (doFOV (x,y)) l'
      lift $ runStateT tellDrawLevel l''
      return l''

runGame :: Level -> RLState -> StdGen -> IO ()
runGame l rls g = 
    do (a,l',rls',g',w) <- return $ runRoguelikeM runTurn l rls g
       mapM_ handleRLDisplayAction w
       case a of
         None -> runGame l' rls' g'
         PlayerAction -> do a' <- readActionFromPlayer
                            case a' of
                              None -> return ()
                              _    -> runGame l' rls'{playerAction = a'} g'
         _ -> error ("Invalid Action at toplevel: " ++ (show a))

handleRLDisplayAction :: RLDisplayAction -> IO ()
handleRLDisplayAction rlda = do case rlda of
                                  PutMessage str   -> putMessage str
                                  UpdateCell p s -> drawSymbol p s
                                  DrawLevel  sa    -> drawSymbolArray sa
                                move 0 0
                                refresh
                             

readActionFromPlayer :: IO Action
readActionFromPlayer = do c <- getCh
                          case c of
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
                            KeyChar 'q' -> return $ None
                            _           -> readActionFromPlayer
                            
putMessage :: String -> IO ()
putMessage str =
    do move 0 0
       clrToEol
       mvWAddStr stdScr 0 0 str
       refresh
       getCh
       return ()
       
      
  
drawSymbol :: (Int,Int) -> Symbol -> IO ()
drawSymbol (x,y) s = mvWAddStr stdScr (y+1) x [c]
    where c = case s of {
                       Blank  -> ' ';
                       Floor  -> '.';
                       Rock   -> '#';
                       HWall  -> '-';
                       VWall  -> '|';
                       Player -> '@'
                     }

drawSymbolArray :: (Array (Int, Int) Symbol) -> IO ()
drawSymbolArray sa = forM_ xs (\x -> drawSymbol x (sa ! x))
    where xs = range ((0,0), (xMax,yMax))

drawLevel :: Level -> IO ()
drawLevel l = forM_ xs (\x -> drawSymbol x (symbolAt l x))
    where xs = range ((0,0), (xMax,yMax))
          
debugMain = 
    let symbolChar s = case s of
                         Blank  -> ' '
                         Floor  -> '.'
                         Rock   -> '#'
                         HWall  -> '-'
                         VWall  -> '|'
                         Player -> '@'
        handleRLDisplayActions a []   = return a
        handleRLDisplayActions a (x:xs) = 
            case x of
              PutMessage str -> do putStrLn str
                                   handleRLDisplayActions a xs
              UpdateCell p s -> handleRLDisplayActions (a//[(p,s)]) xs
              DrawLevel a'   -> handleRLDisplayActions a' xs
        displaySymbolArray a = 
            let drawLine y = 
                    putStrLn $ map (\x -> symbolChar (a!(x,y))) (range (0,xMax))
            in mapM_ drawLine (range (0,yMax))
        getPlayerAction = do c <- getChar
                             case c of 
                               '8' -> return $ Move North
                               'k' -> return $ Move North
                               '6' -> return $ Move East
                               'l' -> return $ Move East
                               '2' -> return $ Move South
                               'j' -> return $ Move South
                               '4' -> return $ Move West
                               'h' -> return $ Move West
                               '9' -> return $ Move NorthEast
                               'u' -> return $ Move NorthEast
                               '7' -> return $ Move NorthWest
                               'y' -> return $ Move NorthWest
                               '3' -> return $ Move SouthEast
                               'n' -> return $ Move SouthEast
                               '1' -> return $ Move SouthWest
                               'b' -> return $ Move SouthWest
                               'q' -> return $ None
                               _           -> getPlayerAction
        runGame l rls g sa = 
            do (a,l',rls',g',w) <- return $ runRoguelikeM runTurn l rls g
               sa' <- handleRLDisplayActions sa w
               displaySymbolArray sa'
               case a of
                 None -> runGame l' rls' g' sa'
                 PlayerAction -> do a' <- getPlayerAction
                                    case a' of
                                      None -> return ()
                                      _    -> runGame l' rls'{playerAction = a'} g' sa'
                 _ -> error ("Invalid Action at toplevel: " ++ (show a))
    in do g <- getStdGen
          (l,_,rls,g',w) <- return $ runRoguelikeM setupGame () (RLState 0 None) g
          sa <- handleRLDisplayActions (array ((0,0), (xMax,yMax)) []) w
          displaySymbolArray sa
          runGame l rls g' sa