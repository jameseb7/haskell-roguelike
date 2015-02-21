module HaskellRoguelike.Symbol where

    import Data.Array
    import Data.Function
    import Data.List

    data Symbol = Visible (Either TerrainSymbol EntitySymbol) | 
                  Explored TerrainSymbol |
                  Unexplored
                  deriving (Eq, Show)
            

    data TerrainSymbol = BlankTerrain | Floor | Rock | HWall | VWall
                         deriving (Eq, Show, Enum, Bounded)

    data EntitySymbol = BlankEntity | Player
                        deriving (Eq, Show, Enum, Bounded)

    -- Converts a TerrainSymbol to a Char for the purpose of debug output
    terrainSymbolToChar :: TerrainSymbol -> Char
    terrainSymbolToChar BlankTerrain = ' '
    terrainSymbolToChar Floor        = '.'
    terrainSymbolToChar Rock         = '#'
    terrainSymbolToChar HWall        = '-'
    terrainSymbolToChar VWall        = '|'

    -- Converts a EntitySymbol to a Char for the purpose of debug output
    entitySymbolToChar :: EntitySymbol -> Char
    entitySymbolToChar BlankEntity = ' '
    entitySymbolToChar Player      = '@'

    -- Converts a Symbol to a Char for the purpose of debug output
    symbolToChar :: Symbol -> Char
    symbolToChar (Visible (Left t))  = terrainSymbolToChar t
    symbolToChar (Visible (Right e)) = entitySymbolToChar e
    symbolToChar (Explored t)        = terrainSymbolToChar t
    symbolToChar Unexplored          = ' '

    arrayToLists :: Array (Int,Int) a -> [[a]]
    arrayToLists = map (map snd) . groupBy ((==) `on` (fst . fst)) . assocs

    showSymbolArray :: Array (Int,Int) Symbol -> [String]
    showSymbolArray = map (map symbolToChar) . transpose . arrayToLists

    printSymbolArray :: Array (Int,Int) Symbol -> IO ()
    printSymbolArray = mapM_ putStrLn . showSymbolArray
