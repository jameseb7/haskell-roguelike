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
       drawLevel l
       refresh
       runGame l rls g'
       endWin

setupGame :: RoguelikeM () Level
setupGame = 
    do
      p <- makePlayer
      l <- makeDefaultLevel
      x <- getRandomR (1, levelWidth-2)
      y <- getRandomR (1, levelHeight-2)
      (_,l') <- lift $ runStateT (addEntity p (x,y)) l
      return l'

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
                            KeyUp       -> return $ Move North
                            KeyChar '6' -> return $ Move East
                            KeyRight    -> return $ Move East
                            KeyChar '2' -> return $ Move South
                            KeyDown     -> return $ Move South
                            KeyChar '4' -> return $ Move West
                            KeyLeft     -> return $ Move West
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
    where xs = range ((0,0), (levelWidth-1,levelHeight-1))

drawLevel :: Level -> IO ()
drawLevel l = forM_ xs (\x -> drawSymbol x (symbolAt l x))
    where xs = range ((0,0), (levelWidth-1,levelHeight-1))
          