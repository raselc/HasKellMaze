module Treasure where

import System.IO


--the main Function--
main :: IO()
main = do
        putStrLn "Welcome to THE TREASURE HUNT\nEnter File Name"
        location <- getLine --user input for the file
        inh <- openFile location ReadMode  -- opens the file
        inpStr <- hGetLine inh --reads a line
        hClose inh  -- close the file
        let len = length inpStr --gets the length of a line
        if len > 0
                then do
                        inh <- openFile location ReadMode -- opens the file
                        list <- getFile inh len []  -- calls getFile to get the contents of the file into a list
                        putStrLn "This is my challenge:"
                        putStrLn (unlines list)  -- prints the list
                        hClose inh  -- closes the file
                        let (sth, update) = pathfinder list 0 0 -- calls the maze for marking
                        
                        if  sth -- checks if the return of the pathFinder is true or not
                                then do
                                        putStrLn "Woo hoo, I found the treasure :-)"
                                        putStrLn (unlines update)  -- prints the maze
                                 else do
                                        putStrLn "Uh oh, I could not find the treasure :-("
                                        putStrLn (unlines update) -- prints the maze
                else
                       error ("MAP Error: Check size at row ") -- prints an error


--Gets the contents of the file and returns a list                                           
getFile :: Handle -> Int -> [[Char]] -> IO [[Char]]
getFile inh len lst = do 
        ineof <- hIsEOF inh -- reads until end of file
        if ineof
                then do
                        let finalList = reverse lst -- reverse the list
                        return finalList
                else do 
                        inpStr <- hGetLine inh
                        let nLen = length inpStr -- checks the length of the line
                        if  len /= nLen
                                then do
                                        error ("MAP Error: Check size at row ")
                                else do
                                        let list' = inpStr:lst
                                        getFile inh nLen list'
                                                                                

                                                                                
--Finds the 'y' in the map                                                                                
findRow ::[String] -> Int -> Int -> Char ->  [String]
findRow (l:ls) x y z 
        | y <= 0 = ((setValue x z l):ls)
        | otherwise = l:findRow ls x (y-1) z 

--Sets the value of the x axis                
setValue x z (l:ls)
        |x <= 0 = (z:ls)
        |otherwise = l:setValue(x-1) z ls

--Matches the value of x and y with z of the map                
checkBlock list x y z
        | list !! y !! x == z = True
        | otherwise = False

--finds the path                                                
pathfinder maze x y
    | x >= length (head maze) = (False, maze)
    | y >= length maze = (False, maze)
    | x < 0 = (False, maze)
    | y < 0 = (False,maze)
    | checkBlock maze x y '+' = (False, maze)
    | checkBlock maze x y '#' = (False,maze)
    | checkBlock maze x y '!' = (False, maze)
    | checkBlock maze x y '@' = (True,maze)
    | fst east  = (True,snd east)
    | fst south = (True, snd south)
    | fst west  = (True, snd west)
    | fst north = (True, snd north)
    | otherwise = (False, findRow (snd north) x y '!')
            where 
                    east  =  pathfinder (findRow maze x y '+' ) (x+1) y
                    south = pathfinder (findRow (snd east) x y '+' ) x (y-1)
                    west  = pathfinder (findRow (snd south) x y '+' ) (x-1) y
                    north = pathfinder (findRow (snd west) x y '+' ) x (y+1)                                  