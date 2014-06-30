module HaskellRoguelike.Level 
    (
     Level,
     levelHeight,
     levelWidth,
     defaultLevel,
     symbolAt
    ) where

import Data.Array

import HaskellRoguelike.Symbol

data Cell = 
    Cell { 
      baseSymbol :: Symbol
    }
          
          
data Level = 
    Level { 
      cells :: Array (Int, Int) Cell
    }
           
levelHeight :: Int
levelHeight = 20
    
levelWidth :: Int
levelWidth = 80

defaultLevel = 
    Level (array ((0,0), (levelWidth-1,levelHeight-1)) 
           [ (p, chooseCell p) | 
             p <- range ((0,0), (levelWidth-1,levelHeight-1)) ])
        where chooseCell (x,y)
                  | x == 0 = Cell VWall
                  | x == levelWidth-1 = Cell VWall
                  | y == 0 = Cell HWall
                  | y == levelHeight-1 = Cell HWall
                  | otherwise = Cell Floor
              
symbolAt :: Level -> (Int, Int) -> Symbol
symbolAt l p = symbol ((cells l) ! p)

symbol :: Cell -> Symbol
symbol c = baseSymbol c