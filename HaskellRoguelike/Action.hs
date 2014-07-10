module HaskellRoguelike.Action where

    data Direction = 
        Here | North | East | South | West |
        NorthEast | NorthWest | SouthEast | SouthWest |
        Up | UpNorth | UpEast | UpSouth | UpWest |
        UpNorthEast | UpNorthWest | UpSouthEast | UpSouthWest |
        Down | DownNorth | DownEast | DownSouth | DownWest |
        DownNorthEast | DownNorthWest | DownSouthEast | DownSouthWest |
        Next | Prev
                        deriving (Eq, Show, Enum, Bounded)

    toOffset :: Direction -> (Int,Int,Int,Int)
    toOffset dir = case dir of
                     Here          -> ( 0, 0, 0, 0)
                     North         -> ( 0,-1, 0, 0)
                     East          -> ( 1, 0, 0, 0)
                     South         -> ( 0, 1, 0, 0)
                     West          -> (-1, 0, 0, 0)
                     NorthEast     -> ( 1,-1, 0, 0)
                     NorthWest     -> (-1,-1, 0, 0)
                     SouthEast     -> ( 1, 1, 0, 0)
                     SouthWest     -> (-1, 1, 0, 0)
                     Up            -> ( 0, 0, 1, 0)
                     UpNorth       -> ( 0,-1, 1, 0)
                     UpEast        -> ( 1, 0, 1, 0)
                     UpSouth       -> ( 0, 1, 1, 0)
                     UpWest        -> (-1, 0, 1, 0)
                     UpNorthEast   -> ( 1,-1, 1, 0)
                     UpNorthWest   -> (-1,-1, 1, 0)
                     UpSouthEast   -> ( 1, 1, 1, 0)
                     UpSouthWest   -> (-1, 1, 1, 0)
                     Down          -> ( 0, 0,-1, 0)
                     DownNorth     -> ( 0,-1,-1, 0)
                     DownEast      -> ( 1, 0,-1, 0)
                     DownSouth     -> ( 0, 1,-1, 0)
                     DownWest      -> (-1, 0,-1, 0)
                     DownNorthEast -> ( 1,-1,-1, 0)
                     DownNorthWest -> (-1,-1,-1, 0)
                     DownSouthEast -> ( 1, 1,-1, 0)
                     DownSouthWest -> (-1, 1,-1, 0)
                     Next          -> ( 0, 0, 0, 1)
                     Prev          -> ( 0, 0, 0,-1)

    data Action = None | PlayerAction |
                  Move Direction
                       deriving (Eq, Show)