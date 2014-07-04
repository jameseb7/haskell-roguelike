module HaskellRoguelike.Action where

    data Direction = 
        Here | North | East | South | West |
        NorthEast | NorthWest | SouthEast | SouthWest |
        Up | UpNorth | UpEast | UpSouth | UpWest |
        UpNorthEast | UpNorthWest | UpSouthEast | UpSouthWest |
        Down | DownNorth | DownEast | DownSouth | DownWest |
        DownNorthEast | DownNorthWest | DownSouthEast | DownSouthWest
                        deriving (Eq, Show, Enum, Bounded)

    data Action = None | PlayerAction |
                  Move Direction
                       deriving (Eq, Show)