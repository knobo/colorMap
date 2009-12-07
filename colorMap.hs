module Main where
import Data.List
import Maybe

--  Types
data Color =  Red | Blue | Green | Purple | None deriving (Eq, Show, Read)
data Country = Finland | Sweden    | Norway      | Russia  | Estonia |
               Latvia  | Lithuania | Kaliningrad | Belarus | Ukraine |
               Poland deriving (Eq, Show, Read)

--  NameOfCountry ColorOfCountry Neighbors PossibleColors ([] means any color you like)
data Node = Node Country Color [Country] [Color] deriving (Eq, Show, Read)

colors :: [Color]
colors = [Red, Blue, Green, Purple]

-- Tests
nameEq :: Country -> Node -> Bool
nameEq name (Node country _ _ _) = name == country

-- Accessors
colorOf :: Node -> Color
colorOf (Node _ color _ _) = color

getName :: Node -> Country
getName (Node name _ _ _) = name

getNeighbors :: [Node] -> Node -> [Node]
getNeighbors graph (Node _ _ neighbors _) = [x | Just x <- map (getNode graph) neighbors]

getNode :: [Node] -> Country -> Maybe Node
getNode graph name = find (nameEq name) graph

-- Setters
setColor :: Color -> Node -> Node
setColor newColor (Node name color neighbors colors) = (Node name newColor neighbors colors)

-- The program
okNode :: [Node] -> Node -> Bool 
okNode graph node = notElem (colorOf node) (map colorOf (getNeighbors graph node))

doColors :: Node -> [Node]
doColors (Node name color neighbors [])     = [ (Node name c neighbors [])     | c <- colors ] 
doColors (Node name color neighbors colors) = [ (Node name c neighbors colors) | c <- colors ]

colorMap :: [Node] -> [[Node]]
colorMap [] = [[]]
colorMap (node:restNodes) = [ y:ys |  y <- doColors node, 
                                     ys <- colorMap restNodes, 
                                     okNode ys y ]

-- Map data
mapData :: [(Country, [Country], [Color])]

mapData =[(Finland,     [Sweden, Norway, Russia],                           []), 
          (Sweden,      [Finland, Norway],                                  []), 
          (Norway,      [Finland, Sweden, Russia],                          []), 
          (Russia,      [Estonia, Finland, Latvia, Lithuania, Belarus, 
                         Ukraine, Norway],                                  []), 
          (Estonia,     [Russia, Latvia],                                   []), 
          (Latvia,      [Russia, Estonia, Lithuania, Belarus],              []), 
          (Lithuania,   [Latvia, Belarus, Poland, Kaliningrad],             []), 
          (Kaliningrad, [Lithuania, Poland],                                []), 
          (Belarus,     [Poland, Lithuania, Latvia, Russia, Ukraine],       []), 
          (Ukraine,     [Poland, Belarus, Russia],                          []), 
          (Poland,      [Kaliningrad, Lithuania, Belarus, Ukraine],         [])]

mkMap :: [(Country, [Country], [Color])] -> [Node]
mkMap nodes = [(Node name None neighbors colors) | (name, neighbors, colors) <- nodes]

theMap = mkMap mapData

-- Printing function
printNode :: Node -> (Country, Color)
printNode (Node name color _ _) = (name, color)

printMap :: [Node] -> [(Country,Color)]
printMap gMap = (map printNode gMap)

printSolution :: [[Node]] -> [[(Country, Color)]]
printSolution graph = map printMap graph

-- run: printSolution (colorMap theMap)
main = print (printMap (head (colorMap theMap)))
-- main = print (printSolution (colorMap theMap))
