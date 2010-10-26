module Main where
import Data.List
import Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import System.Console.ANSI
import System.Environment

-- Types

data Country = Finland | Sweden | Norway | Russia | Estonia |
               Latvia | Lithuania | Kaliningrad | Belarus | Ukraine |
               Poland deriving (Eq, Show, Read)


-- colorsPermitted: [] means any color is allowed
data Node = Node {nameOf::Country, 
                  colorOf::Color, 
                  neighborsOf::[Country], 
                  colorsPermitted::[Color] } deriving (Show)

instance Eq Color where -- Needed for notElem
  Black   == Black   = True
  Red     == Red     = True
  Green   == Green   = True
  Yellow  == Yellow  = True
  Blue    == Blue    = True
  Magenta == Magenta = True
  Cyan    == Cyan    = True
  White   == White   = True
  _       == _       = False

nameEq :: Country -> Node -> Bool
nameEq name node = name == (nameOf node)

getNeighbors :: [Node] -> Node -> [Node]
getNeighbors graph node = [x | Just x <- map (getNode graph) (neighborsOf node)]

getNode :: [Node] -> Country -> Maybe Node
getNode graph name = find (nameEq name) graph

-- The program

doColors :: Node -> [Color]
doColors node@(Node _ _ _ [])     = take 4 [Black .. ]
doColors node@(Node _ _ _ colors) = colors 

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

theMap :: [Node]
theMap = do (name, neighbors, colors) <- mapData 
            return (Node name Black neighbors colors)

printNode :: (Show a, Show a1) => a -> a1 -> IO ()
printNode name color = putStrLn $ (show name) ++ ": " ++ (show color)

printWithColor :: Node -> IO ()
printWithColor (Node name color _ _) = do setSGR [SetColor Foreground Vivid color]
                                          printNode name color

printWithoutColor :: Node -> IO ()
printWithoutColor (Node name color _ _) = printNode name color


printMap :: (a -> IO b) -> [a] -> IO ()
printMap printer map = do mapM_ printer map
                          putStrLn ""

dispatch :: [[Char]] -> (Node -> IO ())
dispatch [] = printWithoutColor
dispatch (arg:xs) = 
  let (Just printer) = mplus (lookup arg [("color", printWithColor)])
                       (Just printWithoutColor) in
  printer
                        

main = do printer <- dispatch <$> getArgs
          forM_ (colorMap theMap) (printMap printer)

