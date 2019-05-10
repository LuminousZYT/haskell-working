module Lib
    ( someFunc
    ) where

import Data.Char
import Control.Monad
import File
import System.Environment

someFunc :: IO ()
someFunc = {- do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn ("What's your last name?")
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ",how are you?" -}


    {- do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            someFunc -}

    {- do
        return ()
        return "HAHAHA"
        line <- getLine
        return "BLAH BLAH BLAH"
        return 4
        putStrLn line -}

        {- do
        a <- return "hell"
        b <- return "yeah!"
        putStrLn $ a ++ " " ++ b -}

        {- do
        let a = "hell"
            b = "yeah"

        putStrLn $ a ++ " " ++ b -}

        {- do putStr "Hey, "
           putStr "I'm "
           putStrLn "Andy!" -}

        {- do putChar 't'
           putChar 'e'
           putChar 'h' -}
            
           {- do print True
              print 2
              print "haha"
              print 3.2 
              print [3,4,3] -}

            {- do 
            c <- getChar
            if c /= ' '
                then do
                    putChar c
                    someFunc
                else return () -}

            {- do
            rs <- sequence [getLine, getLine, getLine]
            print rs -}

            {- forever $ do
            putStr "Give me some input: "
            l <- getLine
            putStrLn $ map toUpper l -}


            {- do
            colors <- forM [1,2,3,4] (\a -> do
                putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
                color <- getLine
                return color)
            putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
            mapM putStrLn colors -}

            {- do
            contents <- getContents
            putStr (shortLinesOnly contents) -}

            do
                (command:args) <- getArgs
                let (Just action) = lookup command dispatch
                action args

{- 柯里化 -}
reverseWords :: String -> String
reverseWords = unwords . map reverse . words

{- putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do 
    putChar x
    putStr xs  -}   

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result