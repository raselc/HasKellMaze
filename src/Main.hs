module Main where
import System.IO
import Data.List
import Text.Show.Functions
import Debug.Trace
import Control.Lens

sumy :: Integer -> Integer
sumy 0 = 0
sumy n = n + sumy (n-1)

summ :: Integer -> Integer
summ n = n + n

isEven :: Integer -> Bool
isEven n
  | n `mod` 2 == 0 = True
  | otherwise  = False
  
sumtwo :: [Integer] -> [Integer]
sumtwo [] = []
sumtwo (x:[]) = [x]
sumtwo (x:(y:zs)) = (x+y) : sumtwo zs

matha :: [Integer] -> Integer 
matha [a] = a
matha [] = error "u suck"
matha (x:_) = x


data Direction = N | W | S | E deriving (Show,Eq)

mazee = ["---#--###----",
        "-----#----##-",
        "####-#-#-#-##",
        "---#---#-#---",
        "-#-####---##-",
        "-#------#----",
        "-############",
        "------------@"]
  
  
leu = []
direc = 'S'
pos = (0,0)

{-
fpath d pos | fst pos == (length maze - 1) = ""
            | snd  (pos) ==0 || (snd ( pos ) == ((length (maze!!0))-1)) = ""
            | rightPossible d pos = "r" ++ ( fpath (rightRotate d) pos )
            | forwardPossible d  pos = "f" ++ ( fpath d (nstep d pos) )
            | True = "l" ++ fpath (leftRotate d) pos
            where nstep :: Direction -> (Int, Int) -> (Int, Int) {-next step-}
                  nstep N (x,y) = (x-1,y)
                  nstep W  (x,y) = (x,y-1) 
                  nstep S (x,y) = (x+1,y) 
                  nstep E  (x,y) = (x,y+1)

                  rightPossible :: Direction -> (Int, Int) -> Bool 
                  rightPossible N (x,y)= (maze !! x)!! (y+1) == '-'
                  rightPossible W (x,y)= (maze !! (x-1))!! y   == '-'              
                  rightPossible S (x,y)= (maze !! x)!! (y-1) == '-'                   
                  rightPossible E (x,y)= (maze !! (x+1))!! y   == '-'

                  rightRotate :: Direction -> Direction 
                  rightRotate N = E
                  rightRotate W = N
                  rightRotate S = W
                  rightRotate E = S

                  forwardPossible :: Direction -> (Int, Int) -> Bool 
                  forwardPossible N (x,y)= ((maze !! (x-1))!! y) == '-' 
                  forwardPossible W (x,y)= ((maze !! x)!! (y-1)) == '-'
                  forwardPossible S (x,y)= ((maze !! (x+1))!! y) == '-'
                  forwardPossible E (x,y)= ((maze !! x)!! (y+1)) == '-'

                  leftRotate :: Direction -> Direction
                  leftRotate N = W
                  leftRotate W = S
                  leftRotate S = E
                  leftRotate E = N


-}

 
 
safeBlock list x y
        | (((x >= 0) && (x < length (head list))) &&  ((y >= 0) && (y < length list))) && ((checkBlock list x y '-') || (checkBlock list x y '@'))  =  True
        | otherwise = False

--Assignment--

--C:/Users/Rasel/workspace/HasKellProgram/src/map.txt
-- ["---#--###----","-#---#----##-","####-#-#-#-##","---#---#-#---","-#-####---##-","-#------#----","-############","------------@"]
--["---#@","--#--","-##-#","-----"]



main = do
        putStrLn "Welcome to THE TREASURE HUNT\nEnter File Name"
        location <- getLine
        putStrLn location
        inh <- openFile location ReadMode
        inpStr <- hGetLine inh
        let len = length inpStr
        if len > 0
                then do
                        hClose inh
                        inh <- openFile location ReadMode
                        list <- getFile inh len []
                        hClose inh
                        let (pathExist, newMaze) = pathFinder list 0 0
                        let (sth, update) = path newMaze 0 0
                        if pathExist 
                                then do
                                        putStrLn "found path"
                                        putStrLn (unlines newMaze)
                                 else do
                                        putStrLn "path not Found"
                                        putStrLn (unlines update)
                                        
                                        
                                        
                      --putStrLn (show (fpath S (0,1) ))
                      -- putStr ( unlines list)
                      --  print (show (findPath 0 0 list))
                        
                else
                       error ("MAP Error: Check size at row ")
                                

        

getFile :: Handle -> Int -> [[Char]] -> IO [[Char]]
getFile inh len lst = do 
        ineof <- hIsEOF inh
        if ineof
                then do
                        let finalList = reverse lst
                        return finalList
                else do 
                        inpStr <- hGetLine inh
                        let nLen = length inpStr
                        if  len /= nLen
                                then do
                                        error ("MAP Error: Check size at row ")-- ++ show( (length lst)+1)) 
                                else do
                                        let list' = inpStr:lst
                                        getFile inh nLen list'

replaceBlock list x y z = list & element x . element y .~ z

findRow ::[String] -> Int -> Int -> Char ->  [String]
findRow (l:ls) x y z 
        | y <= 0 = ((setValue x z l):ls)
        | otherwise = l:findRow ls x (y-1) z 

setValue x z (l:ls)
        |x <= 0 = (z:ls)
        |otherwise = l:setValue(x-1) z ls

checkBlock list x y z
        | list !! y !! x == z = True
        | otherwise = False


-- pathFinder ["---#@","--#--","-##-#","-----"] 0 0
{-
pathFinder maze x y
        | trace(show x ++ " " ++ show y++ show maze) not(safeBlock maze x y) = (False,maze)
        | checkBlock maze x y '#' = (False,maze)
        | checkBlock maze x y '!' = (False, maze)
        | checkBlock maze x y '@' = (True,maze)
        | fst east = (True, snd east)
        | fst south = (True, snd south)
        | fst west = (True, snd west)
        | fst north = (True, snd north)
        | fst noWay = (False,snd noWay)
        | otherwise = (False, maze)
                where 
                        noWay = pathFinder (findRow maze x y '!') x (y+1)
                        north = pathFinder(findRow maze x y '+') x (y-1)
                        south = pathFinder (findRow maze x y '+') x (y+1)
                        east = pathFinder (findRow maze x y '+') (x+1) y
                        west = pathFinder (findRow maze x y '+') (x-1) y
-}


                        
pathFinder maze x y
      --  | trace(show x ++ " " ++ show y++ show maze) not(safeBlock maze x y) = (False,maze)
        | x >= length (head maze) = (False, maze)
        | y >= length maze = (False, maze)
        | x < 0 = (False, maze)
        | y < 0 = (False,maze)
        | checkBlock maze x y '+' = (False, maze)
        | checkBlock maze x y '#' = (False,maze)
        | checkBlock maze x y '!' = (False, maze)
        | checkBlock maze x y '@' = (True,maze)
        | fst east = (True,snd east)
        | fst south = (True, snd south)
        | fst west = (True, snd west)
        | fst north = (True, snd north)
       -- | not (fst north) = (True, snd east)
        | fst noWay = (True, snd noWay)
       -- | fst noNorth = (True, snd noNorth)
       -- | fst noWest = (True, snd noWest)
       -- | fst noSouth = (True, snd noSouth)
       -- | fst noEast = (True, snd noEast)
       -- | otherwise = (False, maze)
          | otherwise = (False, maze)
                where 
                        noWay = pathFinder (findRow maze x y '!') x (y+1)
                        north = pathFinder(findRow maze x y '+') x (y-1)
                        south = pathFinder (findRow maze x y '+') x (y+1)
                        east = pathFinder (findRow maze x y '+') (x+1) y
                        west = pathFinder (findRow maze x y '+') (x-1) y
                      --  noEast = pathFinder (findRow maze x y '!') (x-1) y
                      --  noSouth = pathFinder (findRow maze x y '!') x (y-1)
                      --  noWest = pathFinder (findRow maze x y '!') (x+1) y
                      --  noNorth = pathFinder (findRow maze x y '!') x (y+1)
                        
  
path maze x y
    | x >= length (head maze) = (False, maze)
    | y >= length maze = (False, maze)
    | x < 0 = (False, maze)
    | y < 0 = (False,maze)
    | checkBlock maze x y '+' = (False, maze)
    | checkBlock maze x y '#' = (False,maze)
    | checkBlock maze x y '!' = (False, maze)
    | checkBlock maze x y 'G' = (True,maze)
    | fst east = (True,snd east)
    | fst south = (True, snd south)
    | fst west = (True, snd west)
    | fst north = (True, snd north)
   -- | fst noWay = (True, snd noWay)
    | otherwise = (False, findRow (snd north) x y '!')
            where 
                    noWay = path (findRow maze x y '!') x (y+1)
                   -- north = path(findRow maze x y '+') x (y-1)
                   -- south = path (findRow maze x y '+') x (y+1)
                   -- east = path (findRow maze x y '+') (x+1) y
                   -- west = path (findRow maze x y '+') (x-1) y
                    east =  path (findRow maze x y '+' ) (x+1) y
                    south = path (findRow (snd east) x y '+' ) x (y-1)
                    west = path (findRow (snd south) x y '+' ) (x-1) y
                    north = path (findRow (snd west) x y '+' ) x (y+1)                     