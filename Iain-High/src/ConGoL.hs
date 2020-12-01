--    _____                               _        _____                               __   _      _  __
--   / ____|                             ( )      / ____|                             / _| | |    (_)/ _|
--  | |     ___  _ ____      ____ _ _   _|/ ___  | |  __  __ _ _ __ ___   ___    ___ | |_  | |     _| |_ ___
--  | |    / _ \| '_ \ \ /\ / / _` | | | | / __| | | |_ |/ _` | '_ ` _ \ / _ \  / _ \|  _| | |    | |  _/ _ \
--  | |___| (_) | | | \ V  V / (_| | |_| | \__ \ | |__| | (_| | | | | | |  __/ | (_) | |   | |____| | ||  __/
--   \_____\___/|_| |_|\_/\_/ \__,_|\__, | |___/  \_____|\__,_|_| |_| |_|\___|  \___/|_|   |______|_|_| \___|
--                                   __/ |
--                                  |___/
--                               _____      _         _    _ _       _
--                              |_   _|    (_)       | |  | (_)     | |
--                                | |  __ _ _ _ __   | |__| |_  __ _| |__
--                                | | / _` | | '_ \  |  __  | |/ _` | '_ \
--                               _| || (_| | | | | | | |  | | | (_| | | | |
--                              |_____\__,_|_|_| |_| |_|  |_|_|\__, |_| |_|
--                                                              __/ |
--                                                              |___/
-- Conway's Game of Life
-- Iain High
-- 2020 Inf1A FP Programming Competition

-- For my entry for the Inf1A Programming Competition I have created the fameous Conway's Game of Life in Haskell. Life is an algorithm designed by British mathematician John Conway in 1970. One interacts with the Game of Life by creating an initial configuration and observing how it evolves.

-- Rules: The game is played on a 2D grid of cells each with two possible states, alive or dead. Each cell interacts with its eight neighbours (the eight adjacent cells) at the same time.
--      1) Any live cell with fewer than two live neighbours dies, as if by underpopulation
--      2) Any live cell with two or three live neighbours lives on to the next generation
--      3) Any live cell with more than three live neighbours dies, as if by overpopullation
--      4) Any dead cell with exactly three neighbours becomes an alive cell, as if by reproduction.


---------------------------------------DEPENDENCIES:
import ConGoLDisp -- This is a slightly modified version of "LSystems.hs" from tutorial 7.
import Control.Monad (liftM2)
import Control.Concurrent
import Data.List

---------------------------------------DATA TYPE DECLORATIONS:

type Coordinate = (Int, Int)
-- The Coordinates of any cell.

data Status = Alive
            | Dead
            deriving (Eq, Show)
-- Each cell can either be alive (in which case it will show), or dead (it will be empty). (Schrödinger would be happy).

type Cell = (Coordinate, Status)
-- The full descriptor of each cell including its coordinates and its living status.

type Grid = [Cell]
-- The full grid of cells.

---------------------------------------TEST GRIDS:
emptyGrid = [((x,y), Dead) | x<-[0..((10*2)-1)], y<-[0..((10*2)-1)]]
testGrid1 = (gridEdit (gridEdit (gridEdit emptyGrid (3,2) Alive) (1,2) Alive) (2,2) Alive)
testGrid2 = gridAliveMultiple emptyGrid [(0,2),(1,0),(1,2),(2,2),(2,1),(18,0),(17,1),(17,2),(18,2),(19,2)]
testGrid3 = gridAliveMultiple emptyGrid [(3,0),(1,1),(5,1),(6,2),(1,3),(2,4),(3,4),(4,4),(5,4),(6,3),(6,4)]
testGrid4 = gridAliveMultiple emptyGrid [(9,8),(10,8),(11,8),(10,9),(10,10),(10,11)]
testGrid5 = gridAliveMultiple emptyGrid [(4,8),(5,8),(6,8),(3,9),(3,10),(3,11),(4,12),(5,12),(6,12),(7,9),(7,10),(7,11),(10,8),(11,8),(12,8),(9,9),(9,10),(9,11),(10,12),(11,12),(12,12),(13,9),(13,10),(13,11)]

----------------------------------------GRID INPUT FUNCTIONS

weightTime = 1000
--The time between frames in milliseconds (has to be atleast 1000)

gridAliveMultiple :: Grid -> [Coordinate] -> Grid
gridAliveMultiple gs [] = gs
gridAliveMultiple gs (c:cs) = gridAliveMultiple (gridEdit gs c Alive) cs
-- Takes a current grid and a list of coordinates and sets the inputed coordinates to alive.

gridEdit :: Grid -> Coordinate -> Status -> Grid
gridEdit xs (x1,y1) Alive = ((x1,y1), Alive) : [((x,y),a) | ((x,y),a) <- xs, (x/=x1 || y/=y1)]
gridEdit xs (x1,y1) Dead = ((x1, y1), Dead) : [((x,y),a) | ((x,y),a) <- xs, (x/=x1 || y/=y1)]
-- Used to edit each cell individually, not recommended for input as it can get tedius but is useful if you made a mistake and need to kill a cell again.

---------------------------------------MAIN FUNCTIONS:

runCommandListInfinite :: Grid -> IO [()]
runCommandListInfinite grid = sequence (commandListInfinite grid)
-- Runs infinitely.

commandListInfinite :: Grid -> [IO ()]
commandListInfinite grid = (displayState grid) : (Control.Concurrent.threadDelay (weightTime * 1000)) : (commandListInfinite (update grid) )


runCommandListFixed:: Int -> Grid -> IO [()]
runCommandListFixed n grid = sequence (commandListFixed n grid)
-- Runs Conway's Game of Life for n iterations and then stops. This is the recomended mode so you don't have to kill the program when your finished.

commandListFixed :: Int -> Grid -> [IO ()]
commandListFixed n grid
  | n > 0 = (displayState grid) : (Control.Concurrent.threadDelay (weightTime * 1000)) : (commandListFixed (n-1) (update grid) )
  | otherwise = []

---------------------------------------CALCULATE NEXT GRID:
nextGrid :: Grid -> Grid
nextGrid current = [(f ((x, y), a) current) | ((x , y), a) <- (identifyImportants current)]
  where
  f :: Cell -> Grid -> Cell
  f ((x, y), Dead) current = (rule4 ((x, y), Dead) current)
  f ((x, y), Alive) current = (rule1 ((x, y), Alive) current)
-- Takes the current grid as input and returns the next grid following the rules, only deals with 'important' cells to save processing power - see later functions.

update :: Grid -> Grid
update xs = (gridAliveMultiple emptyGrid [(x,y) | ((x,y),Alive)<-nextGrid xs])
-- updates the grid by calculating all the coordinates that will be alive and then builds a new list with the calculated alive cells and everything else dead.

identifyImportants :: Grid -> Grid
identifyImportants xs = nub (concat [f ((x,y), Alive) | ((x, y), Alive)<-xs])
  where
  f ((x,y), Alive) = [((x1,y1),a) | ((x1,y1),a) <- xs, (x1 == x-1 || x1 == x || x1 == x+1) && (y1 == y-1 || y1 == y || y1 == y+1)]
--Identify the important cells (the cells which are either alive or adjacent to an alive cell, this saves processing some cells)

getListofCoords :: Grid -> [Coordinate]
getListofCoords xs = [(x,y) | ((x,y),Alive) <- xs]
--Get list of living cells

--I know optimiation is bad - but why should I make the poor computer look at all the empty cells that aren't adjacent to any living cell?
--nextGrid :: Grid -> Grid
--nextGrid current = [(f ((x, y), a) current) | ((x , y), a) <- current]
--  where
--  f :: Cell -> Grid -> Cell
--  f ((x, y), Dead) current = (rule4 ((x, y), Dead) current)
--  f ((x, y), Alive) current = (rule1 ((x, y), Alive) current)


---------------------------------------RULES:
-- Any live cell with fewer than two live neighbours dies, as if by underpopulation
rule1 :: Cell -> Grid -> Cell
rule1 ((x, y), Alive) current
  | (len ((x, y), Alive) current) < 2 = ((x, y), Dead)
  | otherwise = rule2  ((x, y), Alive) current

-- Any live cell with two or three live neighbours lives on to the next generation
rule2 :: Cell -> Grid -> Cell
rule2 ((x, y), Alive) current
  | (len ((x, y), Alive) current) == 2 || (len ((x, y), Alive) current) == 3 = ((x, y), Alive)
  | otherwise = ((x, y), Dead)
  -- If it's alive and it's not effected by rule 1 or rule 2, then it must be effected by rule 3 and hence dies.

-- Any dead cell with exactly three neighbours becomes an alive cell, as if by reproduction.
rule4 :: Cell -> Grid -> Cell
rule4 ((x, y), Dead) current
  | len((x, y), Dead) current  == 3 = ((x, y), Alive)
  | otherwise = ((x, y), Dead)

len :: Cell -> Grid -> Int
len ((x, y), a) current = length [((x1, y1), a) | ((x1, y1), Alive) <- current, (x1 == x-1 || x1 == x || x1 == x+1) && (y1 == y-1 || y1 == y || y1 == y+1) && (y1/=y || x1/=x)]
-- Do I get extra brownie points for the cheeky wee CNF thrown in there? The number of living neighbourghs that the cell has.

---------------------------------------DISPLAY FUNCTIONS:

displayState :: Grid -> IO ()
displayState x = displayWithSquares (displayGrid 10) (displaySquares x)

displayGrid :: Int -> Command
displayGrid num = (Branch (copy num (Go 10 :#: Turn (-90) :#: GrabPen Inkless :#: Go (5 / (fromIntegral num)) :#: GrabPen black :#: Turn (-90) :#: Go 10 :#: Turn (90) :#: GrabPen Inkless :#:  Go (5 / (fromIntegral num)) :#: GrabPen black :#: Turn (90)) :#: Go 10)) :#: GrabPen Inkless :#: Go 10 :#: Turn (270) :#: GrabPen black :#: (Branch (copy num (Go 10 :#: Turn (-90) :#: GrabPen Inkless :#: Go (5 / (fromIntegral num)) :#: GrabPen black :#: Turn (-90) :#: Go 10 :#: Turn (90) :#: GrabPen Inkless :#:  Go (5 / (fromIntegral num)) :#: GrabPen black :#: Turn (90)) :#: Go 10))

displaySquares :: Grid -> String
displaySquares [] = []
displaySquares (((x,y),Dead):ys) = displaySquares ys
displaySquares (((x,y),Alive):ys) = "<rect x=\" " ++  xposstr ++ "% \" y= \" " ++ yposstr ++ "% \" width=\" " ++ widthstr ++ "% \" height=\" " ++ widthstr ++ "% \" style=\"fill:#f5ff66\"/>" ++ (displaySquares ys)
  where
  width :: Double
  width = (100 / fromIntegral(2*10+2))
  widthstr = show (width)
  xpos :: Double
  xpos = (((100-(2*width)) * ((fromIntegral x) / 2)) / (fromIntegral 10)) + width
  xposstr = show (xpos)
  ypos :: Double
  ypos = (((100-(2*width))  * ((fromIntegral y) / 2)) / (fromIntegral 10)) + width
  yposstr = show (ypos)
