module IRC where 

data ServerMessage = CMD String String [String] String | NOTICE | NUMERIC Int [String] deriving (Show, Eq)
