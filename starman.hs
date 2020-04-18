{-# LANGUAGE ViewPatterns #-}

import Data.List(intersperse)

space = ' '
underscore = '_'
messageLost = "You Lose"
messageWon = "You Win!!"
messageMakeGuess = "Enter your guess: "

starman :: String -> Int -> IO()
starman word turns  = turn letters turns
    where letters   = [(letter, False) | letter <- word]

turn :: [(Char, Bool)] -> Int -> IO()
turn _ 0                = putStrLn messageLost
turn (hasWon -> True) _ = putStrLn messageWon
turn letters turnsLeft  =
    do
        putStrLn (formatProgress letters turnsLeft)
        guess <- askGuess
        if checkGuess letters guess
            then turn (updateProgress letters guess) turnsLeft
            else turn letters (turnsLeft - 1)

hasWon :: [(Char, Bool)] -> Bool
hasWon = all snd

formatProgress :: [(Char, Bool)] -> Int -> String
formatProgress letters turnsLeft =
    formatWord letters ++
    [space] ++
    formatTurnsLeft turnsLeft

formatTurnsLeft :: Int -> String
formatTurnsLeft turns = take turns (repeat '*')

formatWord :: [(Char, Bool)] -> String
formatWord letters = intersperse
    space 
    [if guessed
        then character
        else underscore | (character, guessed) <- letters]

askGuess :: IO(Char)
askGuess = do 
    putStr messageMakeGuess
    guess <- getLine
    return (guess!!0)

checkGuess :: [(Char, Bool)] -> Char -> Bool
checkGuess letters guess = elem guess (map fst letters)

updateProgress :: [(Char, Bool)] -> Char -> [(Char, Bool)]
updateProgress [] _ = []
updateProgress ((letter, guessed):xs) guess 
    | letter == guess   = (letter, True):(updateProgress xs guess)
    | otherwise         = (letter, guessed):(updateProgress xs guess)
