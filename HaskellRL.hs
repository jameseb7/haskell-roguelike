import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import System.Random
import Data.Ix

import UI.HSCurses.Curses

import HaskellRoguelike.Action
import HaskellRoguelike.Entity
import HaskellRoguelike.Level
import HaskellRoguelike.State
import HaskellRoguelike.Symbol



main = 
    do 
      initScr
      echo False
      cBreak True
      nl False
      g <- getStdGen
      (l,_,rls,g',w) <- return $ runRoguelikeM setupGame () (RLState 0 None) g
      drawLevel l
      refresh
      getCh
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
  
drawSymbol :: (Int,Int) -> Symbol -> IO ()
drawSymbol (x,y) s = mvWAddStr stdScr y x [c]
    where c = case s of {
                       Blank  -> ' ';
                       Floor  -> '.';
                       Rock   -> '#';
                       HWall  -> '-';
                       VWall  -> '|';
                       Player -> '@'
                     }

drawLevel :: Level -> IO ()
drawLevel l = forM_ xs (\x -> drawSymbol x (symbolAt l x))
    where xs = range ((0,0), (levelWidth-1,levelHeight-1))
          