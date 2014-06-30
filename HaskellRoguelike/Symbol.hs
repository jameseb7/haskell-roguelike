module HaskellRoguelike.Symbol where

data Symbol = Blank | Floor | Rock | HWall | VWall
            deriving (Eq, Show, Enum, Bounded)