-- Proj1.hs
-- Author: Haichao Song 854035
-- Answer for COMP30020 Declarative Programming Project 1: Guessing Cards

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
import qualified Data.Set

----
-- Generate, update and revise game state
----

-- Game state stores card numbers and options we could the answer might be
data GameState = GameState {
    guessNum :: Int,
    guessOption :: [[Card]]
} deriving (Show)

-- Generate options for guesses and answers for initial guess
getOption :: Int -> [Card] -> [[Card]]
getOption 0 _ = [[]]
getOption _ [] = []
getOption n (x : xs) = map (x :) (getOption (n - 1) xs) ++ getOption n xs

-- Check if the option has repeat cards inside
validOption :: Eq a => [a] -> Bool
validOption [] = True
validOption (x:xs) = (notElem x xs) && validOption xs

----
-- For the feedback according to guess and answer
----

-- Check if guess and answer have same cards
samecards :: [Card] -> [Card] -> Int
samecards [] _ = 0
samecards _ [] = 0
samecards (x:xs) (y:ys)
    | x == y = 1 + samecards xs ys
    | otherwise = (samecards [x] ys) + samecards xs (y:ys)

-- Return the lowest rank in a list of cards
lowestrank :: [Card] -> Rank
lowestrank [] = error "No card given"
lowestrank [Card s r] = r
lowestrank ((Card s r) : xs) = min r (lowestrank xs)

-- Check the number of cards in the answer have rank lower 
-- than the lowest rank in the guess
lowernum :: [Card] -> [Card] -> Int
lowernum [] _ = 0
lowernum _ [] = 0
lowernum ((Card s r):xs) ys
    | r < (lowestrank ys) = 1 + lowernum xs ys
    | otherwise = lowernum xs ys

-- Check the number of cards in the answer have the same rank as the guess
matchnum :: [Card] -> [Card] -> Int
matchnum [] _ = 0
matchnum _ [] = 0
matchnum ((Card s1 r1):xs) ((Card s2 r2):ys)
    | r1 == r2 = 1 + matchnum xs ys
    | otherwise = (matchnum [(Card s1 r1)] ys) + matchnum xs ((Card s2 r2):ys)


-- Return the highest rank in a list of cards
highestrank :: [Card] -> Rank
highestrank [] = error "No card given"
highestrank [Card s r] = r
highestrank ((Card s r) : xs) = max r (highestrank xs)

-- Check the number of cards in the answer have rank higher
-- than the highest rank in the guess
highernum :: [Card] -> [Card] -> Int
highernum [] _ = 0
highernum _ [] = 0
highernum ((Card s r):xs) ys
    | r > (highestrank ys) = 1 + highernum xs ys
    | otherwise = highernum xs ys

-- Check the number of cards in the answer have the same suit as the guess
samesuit :: [Card] -> [Card] -> Int
samesuit [] _ = 0
samesuit _ [] = 0
samesuit ((Card s1 r1):xs) ((Card s2 r2):ys)
    | s1 == s2 = 1 + samesuit xs ys
    | otherwise = (samesuit [(Card s1 r1)] ys) + samesuit xs ((Card s2 r2):ys)

-- Generate feedback for guess
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback [] _ = (0,0,0,0,0)
feedback _ [] = (0,0,0,0,0)
feedback xs ys = (samecards xs ys, lowernum xs ys, matchnum xs ys, highernum xs ys, samesuit xs ys)

----
-- For initial guess part
----

-- Give initial guess and generate game state according to cards number
initialGuess :: Int -> ([Card],GameState)
initialGuess 0 = error "Cannot guess 0 card"
initialGuess n = (cards, state)
    where 
    state = GameState {guessNum=n, guessOption=(getOption n card)}
        where card = [(Card Club R2) .. (Card Spade Ace)]
    cards = zipWith Card suits ranks
        where 
        suits = take n [Club ..]
        ranks = averageNth n [R2 ..]

-- return the initial guess cards rank       
averageNth :: Int -> [t] -> [t]
averageNth _ [] = []
averageNth n xs = last (take a xs):(averageNth n (drop a xs))
    where a = div 13 (n+1)


----
-- For the next guess part
----

-- Give next guess and update game state according to the feedback and last guess
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (x, GameState {guessNum=n, guessOption=go}) fd = 
    (a, GameState {guessNum=n,guessOption=b})
    where 
    b = optionCheck go x fd
    a = get2 (head (sort (bestGuess b)))
        where get2 (_, n) = n

-- Update options fit for the new feedback 
optionCheck :: [[Card]] -> [Card] -> (Int,Int,Int,Int,Int) -> [[Card]]
optionCheck [] _ _ = []
optionCheck (x:xs) ys fb 
    | (feedback x ys) == fb = (x:(optionCheck xs ys fb))
    | otherwise = optionCheck xs ys fb

-- Get feedbacks of all possible answerrs for a specific guess
getFeedbacks :: [Card] -> [[Card]]-> [(Int,Int,Int,Int,Int)]
getFeedbacks [] _ = []
getFeedbacks _ [] = []
getFeedbacks x (y:ys)= ((feedback x y):(getFeedbacks x ys))

-- Get the number of occurrence of each feedback in feedbacks
getFrequency :: [(Int,Int,Int,Int,Int)] -> [(Int, [(Int,Int,Int,Int,Int)])]
getFrequency [] = []
getFrequency s = sort (map (\x -> (length x, [head x])) . group . sort $ s)

-- Calcultate expected values for each guess option
getScore :: [(Int, [(Int,Int,Int,Int,Int)])] -> Double 
getScore [] = 0
getScore a = (fromIntegral (squareAddScore a)) / (fromIntegral (addScore a))

addScore :: [(Int, [(Int,Int,Int,Int,Int)])] -> Int
addScore [] = 0
addScore ((n, s):xs) = n + addScore xs

squareAddScore :: [(Int, [(Int,Int,Int,Int,Int)])] -> Int
squareAddScore [] = 0
squareAddScore ((n, s):xs) = n*n + squareAddScore xs

-- Return the best guess option in all possible guesses using the formula above
bestGuess :: [[Card]] -> [(Double, [Card])]
bestGuess [] = []
bestGuess (x:xs) 
    | (length (x:xs)) > 1500 = [(1, middle (x:xs))]
    | otherwise = ((getScore $ getFrequency $ getFeedbacks x (x:xs), x):(bestGuess xs))
    

middle :: [a] -> a
middle [] = error "empty list"
middle xs = last (take (div ((length xs)+1) 2) xs)