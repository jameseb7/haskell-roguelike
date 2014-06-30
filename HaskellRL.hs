import Control.Monad
import Data.Ix

import UI.HSCurses.Curses

import HaskellRoguelike.Level
import HaskellRoguelike.Symbol


main = 
    do 
      initScr
      echo False
      cBreak True
      nl False
      drawLevel defaultLevel
      refresh
      getCh
      endWin
  
drawSymbol :: (Int,Int) -> Symbol -> IO ()
drawSymbol (x,y) s = mvWAddStr stdScr y x [c]
    where c = case s of {
                       Blank -> ' ';
                       Floor -> '.';
                       Rock  -> '#';
                       HWall -> '-';
                       VWall -> '|'
                     }

drawLevel :: Level -> IO ()
drawLevel l = forM_ xs (\x -> drawSymbol x (symbolAt l x))
    where xs = range ((0,0), (levelWidth-1,levelHeight-1))
          