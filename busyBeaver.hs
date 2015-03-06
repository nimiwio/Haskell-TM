import Turing

busyBeaver2 :: TM
busyBeaver2 = initializeTM states alphabet transition 
    where
        states = [state0,state1,haltState]
        state0 = TMState 0 NoStateType
        state1 = TMState 1 NoStateType
        haltState = TMState 2 AcceptState
        alphabet = map TMSymbol "01"
        transition state (TMSymbol symbol) = 
            case stateID state of
                0 -> case symbol of
                    '0' -> (state1, TMSymbol '1', R)
                    '1' -> (state1, TMSymbol '1', L)
                1 -> case symbol of
                    '0' -> (state0, TMSymbol '1', L)
                    '1' -> (haltState, TMSymbol '1', R)
                _ -> error "Halt state"

busyBeaver3 :: TM
busyBeaver3 = initializeTM states alphabet transition 
    where
        states = [state0,state1,state2,haltState]
        state0 = TMState 0 NoStateType
        state1 = TMState 1 NoStateType
        state2 = TMState 2 NoStateType
        haltState = TMState 3 AcceptState
        alphabet = map TMSymbol "01"
        transition state (TMSymbol symbol) = 
            case stateID state of
                0 -> case symbol of
                    '0' -> (state1, TMSymbol '1', R)
                    '1' -> (haltState, TMSymbol '1', R)
                1 -> case symbol of
                    '0' -> (state2, TMSymbol '0', R)
                    '1' -> (state1, TMSymbol '1', R)
                2 -> case symbol of
                    '0' -> (state2, TMSymbol '1', L)
                    '1' -> (state0, TMSymbol '1', L)
                _ -> error "Halt state"

busyBeaver4 :: TM
busyBeaver4 = initializeTM states alphabet transition 
    where
        states = [state0,state1,state2,state3,haltState]
        state0 = TMState 0 NoStateType
        state1 = TMState 1 NoStateType
        state2 = TMState 2 NoStateType
        state3 = TMState 3 NoStateType
        haltState = TMState 4 AcceptState
        alphabet = map TMSymbol "01"
        transition state (TMSymbol symbol) = 
            case stateID state of
                0 -> case symbol of
                    '0' -> (state1, TMSymbol '1', R)
                    '1' -> (state1, TMSymbol '1', L)
                1 -> case symbol of
                    '0' -> (state0, TMSymbol '1', L)
                    '1' -> (state2, TMSymbol '0', L)
                2 -> case symbol of
                    '0' -> (haltState, TMSymbol '1', R)
                    '1' -> (state3, TMSymbol '1', L)
                3 -> case symbol of
                    '0' -> (state3, TMSymbol '1', R)
                    '1' -> (state0, TMSymbol '0', R)
                _ -> error "Halt state"
