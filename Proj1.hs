--------------------------------------------------------------------------------
-- Author: Qini ZHANG
-- Student Number: 901051
-- Email: qiniz@student.unimelb.edu.au

-- This file is created and edited for COMP30020 Project 1 submission
-- As required, types and functions are exported, which form
-- a complete strategy for the Guess suspects game.
-- The guessing game, where the computer tries to guess the lineup of two
-- different people you specify. Each person is a three letter abbreviation
-- of a person's height, hair colour and sex, in that order. Height is one
-- of S or T; hair colour is one of B, R, or D; and sex is one of M or F.
--------------------------------------------------------------------------------

module Proj1 (Person, parsePerson, height, hair, sex,
              GameState, initialGuess, nextGuess, feedback) where

import Data.List
import Data.Function
import Control.Monad

-------------------------- helpers and types definitions -----------------------
-- define specific type names (improvement)
type Guess = [Person]
type Response = (Int, Int, Int, Int)

-- Since String is used to represent Person, creating a list of suspects
potentialGuess :: [String]
potentialGuess = [[height, hair, sex] | height <- "ST",
                                        hair <- "BRD", sex <- "MF"]

-- List of possible combinations of suspects
allGuess :: [Guess]
allGuess = do
    x <- potentialGuess
    y <- potentialGuess
    guard (x /= y)
    return [x, y]

-- the expected score: (from Hint)
-- Giveing the expected number of remaining possible lineups for each guess
expectScore :: Guess -> GameState -> Double
expectScore target gamestate = do
    let
        -- generate all responses based on target's feedback and current guess
        response = map (feedback target) gamestate
        -- count the number of repetitions for each response.
        scores = map (\x -> length $ pickSame x) $ nub response
            where pickSame x = filter (==x) response

        -- rewrite the operator to divide two itegers and give a result in
        -- floationg point form:
        (//) = (/) `on` fromIntegral

    -- then as the hint, calculate the score.
    sum (map (\x -> x * x) scores) // length scores
--------------------------------------------------------------------------------


--------------- functions/variables/types exported from the module -------------
-- type represents the remaining potential targets, which is game state.
type GameState = [Guess]
-- represent a suspect
type Person = String

-- takes a three-character string and return Just this person or Nothing
parsePerson :: String -> Maybe Person
parsePerson input
    | input `elem` potentialGuess = Just input
    | otherwise = Nothing

height :: Person -> String
height [h, _, _] = [h]

hair :: Person -> String
hair [_, hr, _] = [hr]

sex :: Person -> String
sex [_, _, s] = [s]

-- Takes first a list of the true culprits and second a list of the suspects in
-- the lineup, and returns a quadruple of correct suspects, correct heights,
-- correct hair colours, and correct sexes, in that order.
feedback :: Guess -> Guess -> Response
feedback target guess = do
    let len = length guess

        -- number of "correct Culprit"
        cc = length $ guess `intersect` target

        -- check correct height, hair and sex
        ch = len - cc - length (deleteFirstsBy ((==) `on` head) guess target)
        chr = len - cc - length (deleteFirstsBy ((==) `on` (!!1)) guess target)
        cs = len - cc - length (deleteFirstsBy ((==) `on` (!!2)) guess target)
    (cc, ch, chr, cs)

-- initial seed of my guess
initialGuess :: (Guess,GameState)
initialGuess = (["SBM", "SRM"], allGuess)

-- takes as input a pair of the previous guess and game state (as returned by
-- initialGuess and nextGuess), and the feedback to this guess as a quadruple
-- of correct suspects, correct height, correct hair colour, and correct sex,
-- in that order, and returns a pair of the next guess and new game state.
nextGuess :: (Guess, GameState) -> Response -> (Guess, GameState)
nextGuess (guess, gamestate) response = (g, newGameState)
    where
        -- first adjust the new game state by only to select lineups that are
        -- consistent with the answers received for previous lineups
        -- by computing the list of all possible lineups, and removing elements
        -- that are inconsistent with any answers received to previous lineups
          newGameState
            = filter (\target -> feedback target guess == response) gamestate

        -- select a lineup that gives the minimum expected number of
        -- remaining candidates.
          (_, g)
            = minimum $ map (\x -> (expectScore x newGameState, x)) newGameState
