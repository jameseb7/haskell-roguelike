module HaskellRoguelike.Symbol where

data Symbol = Blank | Floor | Rock | HWall | VWall |
              Player
            deriving (Eq, Show, Enum, Bounded)