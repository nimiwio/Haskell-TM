module Turing where

import Data.List

-- A TM represents a Turing machine at some point in a computation, consisting of a list of states, the current State, the alphabet, and the tape
data TM = TM [TMState] TMState  [TMSymbol] TMTape Transition
getCurrentState :: TM -> TMState
getCurrentState (TM _ currentState _ _ _) = currentState

-- A TMState represents a Turing machine state, consisting of a unique Int identifying the state and a Bool representing whether it is an accepting state.
data TMState = TMState Int TMStateType

stateID :: TMState -> Int
stateID (TMState stateNumber _) = stateNumber

tmStateType :: TMState -> TMStateType
tmStateType (TMState _ stateType) = stateType

isHaltState :: TMState -> Bool
isHaltState state = case tmStateType state of
    RejectState -> True
    AcceptState -> True
    _ -> False

-- A TMStateType represents whether a particular state is an accepting state, a rejecting state, or neither.
data TMStateType = AcceptState | RejectState | NoStateType
    deriving (Show)

-- A TMTape represents a bi-directional tape of infinite capacity.
data TMTape = TMTape TMSymbol [TMSymbol] [TMSymbol]

data TMSymbol = TMSymbol Char | InfSym deriving (Eq)

symChar :: TMSymbol -> Char
symChar (TMSymbol c) = c
symChar InfSym = error "Not a valid symbol"

-- A Transition represents the transition function
type Transition = TMState -> TMSymbol -> (TMState, TMSymbol, Direction)

data Direction = L | R

-- Creates a blank tape using the given Char as a blank
blankTape :: Char -> TMTape 
blankTape c = TMTape blank [InfSym] [blank,InfSym]
   where blank = TMSymbol c

-- Creates a tape with the given String as the initial input, and uses Char parameter as the blank
makeTape :: Char -> String -> TMTape
makeTape blank input = TMTape (TMSymbol blank) [InfSym] $ map TMSymbol input ++ [InfSym]

getTape :: TM -> TMTape
getTape (TM _ _ _ tape _) = tape

-- Returns the symbol in the position of the head of the tape
getTapeHead :: TMTape -> TMSymbol
getTapeHead (TMTape _ _ (x:_)) = x

-- Creates a string representation of the tape
getTapeString :: TMTape -> String
getTapeString tape = case tape of
    TMTape (TMSymbol blank) left (TMSymbol currentChar:right) -> "...|" ++ leftSide ++ "|" ++ emphasize currentChar ++ "|" ++ rightSide ++ "|..."
        where 
            leftSide = blankPadding ++ if null partialLeft then [] else "|" ++ partialLeft 
            rightSide = (if null partialRight then [] else partialRight ++ "|") ++ blankPadding
            partialLeft = reverse (partialContents left) 
            partialRight = partialContents right  
            partialContents = intersperse '|' . map symChar . takeWhile (/= InfSym) 
            blankPadding = intersperse '|' $ replicate 3 blank
            emphasize c = "*" ++ [c] ++ "*" 
    _ -> "Invalid tape"
    
-- Moves and writes to the tape given as input
updateTape :: TMSymbol -> Direction -> TMTape -> TMTape
updateTape newSymbol direction tape =
    moveTape direction $ writeTape newSymbol tape

-- TODO clean this up
moveTape :: Direction -> TMTape -> TMTape
moveTape R (TMTape blank left (symbol:sym2:right)) = case sym2 of
    TMSymbol _ -> TMTape blank (symbol:left) (sym2:right)
    InfSym -> TMTape blank (symbol:left) (blank:sym2:right) 

moveTape L (TMTape blank (symbol:left) right) = case symbol of
    TMSymbol _ -> TMTape blank left (symbol:right)
    infSym -> TMTape blank (infSym:left) (blank:right)

-- TODO compress tape if write a blank to fringe of tape
writeTape :: TMSymbol -> TMTape -> TMTape
writeTape symbol (TMTape blank left (_:right))= TMTape blank left (symbol:right)

runTM :: TM -> IO ()
runTM tm = do
    putStrLn $ getTapeString $ getTape tm
    let newTM = stepTM tm
        newState = getCurrentState newTM
    if isHaltState newState
    then do
        putStrLn $ getTapeString $ getTape newTM
        putStrLn $ "Halted in state " ++ show (stateID newState) ++ ": " ++ show (tmStateType newState)
    else runTM newTM

stepTM :: TM -> TM
stepTM (TM allStates currentState alphabet tape transition) =
    let (newState, newSymbol, direction) = transition currentState $ getTapeHead tape
        newTape = updateTape newSymbol direction tape
    in TM allStates newState alphabet newTape transition

-- Creates a new Turing machine with a blank tape, using the first state in the list of TMState as the initial state, and the first symbol in the list of TMSymbol as the blank symbol
initializeTM :: [TMState] -> [TMSymbol] -> Transition -> TM
initializeTM states@(initState:_) alphabet@(TMSymbol blank:_) transition = TM states initState alphabet (blankTape blank) transition

-- TODO cleanup
validateTM :: TM -> [String]
validateTM (TM allStates currentState alphabet tape transition) = errors
    where
        errors = filter (not . null) [duplicateStatesErrors, invalidAlphabetErrors, noHaltingStateErrors]
        duplicateStatesErrors = let duplicateStates = (map head $ filter ((/=1) . length) $ group $ sort $ map stateID allStates)
                                in if null duplicateStates
                                   then ""
                                   else "The following are duplicate states: " ++ show duplicateStates
        invalidAlphabetErrors = ""
        noHaltingStateErrors = let haltingStates = filter isHaltState allStates
                               in if null haltingStates
                                  then "There are no halting states"
                                  else ""
