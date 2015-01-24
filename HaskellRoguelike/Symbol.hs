module HaskellRoguelike.Symbol where

data Symbol = Visible (Either TerrainSymbol EntitySymbol) | 
              Explored TerrainSymbol |
              Unexplored
              deriving (Eq, Show, Read)
            

data TerrainSymbol = BlankTerrain | Floor | Rock | HWall | VWall
                     deriving (Eq, Show, Read, Enum, Bounded)

data EntitySymbol = BlankEntity | Player
                    deriving (Eq, Show, Read, Enum, Bounded)