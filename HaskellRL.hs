import UI.HSCurses.Curses

main = 
    do 
      initScr
      echo False
      cBreak True
      nl False
      mvWAddStr stdScr 1 1 "Hello, world!"
      refresh
      getCh
      endWin
  
