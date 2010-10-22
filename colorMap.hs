module Main where
import Data.List
import Maybe

-- Types
data Color = Red | Blue | Green | Purple | None deriving (Eq, Show, Read)
data Country = Finland | Sweden | Norway | Russia | Estonia |
               Latvia | Lithuania | Kaliningrad | Belarus | Ukraine |
               Poland deriving (Eq, Show, Read)

colors :: [Color]
colors = [Red, Blue, Green, Purple]

-- colorsPermitted: [] means any color from global colors
data Node = Node {nameOf::Country, 
                  colorOf::Color, 
                  neighborsOf::[Country], 
                  colorsPermitted::[Color] } deriving (Eq, Show, Read)

nameEq :: Country -> Node -> Bool
nameEq name node = name == (nameOf node)

getNeighbors :: [Node] -> Node -> [Node]
getNeighbors graph node = [x | Just x <- map (getNode graph) (neighborsOf node)]

getNode :: [Node] -> Country -> Maybe Node
getNode graph name = find (nameEq name) graph

-- The program

doColors :: Node -> [Color]
doColors node@(Node _ _ _ []) =     [ c | c <- colors ]
doColors node@(Node _ _ _ colors) = [ c | c <- colors ]

colorMap :: [Node] -> [[Node]]
colorMap [] = [[]]
colorMap (node:restNodes) = [ node {colorOf = color}:theMap | theMap <- colorMap restNodes
                                                            , let colorHood = (map colorOf (getNeighbors theMap node)),
                                                              color <- doColors node,
                                                              color `notElem` colorHood ]
-- Map data
mapData :: [(Country, [Country], [Color])]
mapData =[(Finland,     [Sweden, Norway, Russia],  []),
          (Sweden,      [Finland, Norway],         []),
          (Norway,      [Finland, Sweden, Russia], []),
          (Russia,      [Estonia, Finland, Latvia, Lithuania, Belarus,
                         Ukraine, Norway], []),
          (Estonia,     [Russia, Latvia],  []),
          (Latvia,      [Russia, Estonia, Lithuania, Belarus],  []),
          (Lithuania,   [Latvia, Belarus, Poland, Kaliningrad], []),
          (Kaliningrad, [Lithuania, Poland], []),
          (Belarus,     [Poland, Lithuania, Latvia, Russia, Ukraine], []),
          (Ukraine,     [Poland, Belarus, Russia], []),
          (Poland,      [Kaliningrad, Lithuania, Belarus, Ukraine],   [])]

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
-- main = do print (printMap (head (colorMap theMap)))
--       print (printMap (last (colorMap1 theMap)))

main = do print (printSolution (colorMap theMap))
