module HaskellRoguelike.Symbol where

data Symbol = Visible (Either TerrainSymbol EntitySymbol) | 
              Explored TerrainSymbol |
              Unexplored
              deriving (Eq, Show)
            

data TerrainSymbol = BlankTerrain | Floor | Rock | HWall | VWall
                     deriving (Eq, Show, Enum, Bounded)

data EntitySymbol = BlankEntity | Player
                    deriving (Eq, Show, Enum, Bounded)