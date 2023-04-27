module Logic.Interpreter exposing (extract, run)

import Array exposing (Array, foldl, fromList, repeat)
import Array.Extra exposing (update)
import Html exposing (code)
import Html.Attributes exposing (action, list)
import List exposing (append)
import String exposing (isEmpty, slice)
import String exposing (fromInt)



-- MODEL


type Operator
    = RightShift
    | LeftShift
    | Increment
    | Decrement
    | Output


type Action
    = Command Operator
    | Loop String


type alias State =
    { pointer : Int
    , stack : Array Int
    , output : String
    , step : Int
    }


type alias Extractor =
    { depth : Int
    , index : Int
    }


rightShiftBounded : Int -> Int
rightShiftBounded pointer =
    if pointer == 299 then
        0

    else
        pointer + 1


leftShiftBounded : Int -> Int
leftShiftBounded pointer =
    if pointer == 0 then
        299

    else
        pointer - 1


rightShift : State -> State
rightShift state =
    { state | pointer = rightShiftBounded state.pointer }


leftShift : State -> State
leftShift state =
    { state | pointer = leftShiftBounded state.pointer }


incrementBounded : Int -> Int
incrementBounded num =
    if num == 127 then
        -128

    else
        num + 1


decrementBounded : Int -> Int
decrementBounded num =
    if num == -128 then
        127

    else
        num - 1


increment : State -> State
increment state =
    { state | stack = state.stack |> update state.pointer incrementBounded }


decrement : State -> State
decrement state =
    { state | stack = state.stack |> update state.pointer decrementBounded }


output : State -> State
output state =
    { state | output = state.output ++ (fromState state |> fromCode) }


parse : String -> List Action -> List Action
parse input list =
    if isEmpty input then
        list

    else
        let
            code =
                slice 0 1 input

            remains =
                slice 1 (String.length input) input
        in
        case code of
            "[" ->
                let
                    len =
                        extract input (Extractor 0 0)

                    comands =
                        slice 1 len input

                    extracted =
                        slice (len + 1) (String.length input) input
                in
                parse extracted (append list [ Loop comands ])

            ">" ->
                parse remains (append list [ Command RightShift ])

            "<" ->
                parse remains (append list [ Command LeftShift ])

            "+" ->
                parse remains (append list [ Command Increment ])

            "-" ->
                parse remains (append list [ Command Decrement ])

            "." ->
                parse remains (append list [ Command Output ])

            _ ->
                parse remains list


extract : String -> Extractor -> Int
extract codes extractor =
    let
        next =
            extractor.index + 1

        code =
            slice extractor.index next codes
    in
    case code of
        "[" ->
            extract codes (Extractor (extractor.depth + 1) next)

        "]" ->
            if extractor.depth == 1 then
                extractor.index

            else
                extract codes (Extractor (extractor.depth - 1) next)

        _ ->
            extract codes (Extractor extractor.depth next)


fromState : State -> Int
fromState state =
    Array.get state.pointer state.stack |> Maybe.withDefault 0


fromCode : Int -> String
fromCode i =
    Char.fromCode i |> String.fromChar


execute : Operator -> State -> State
execute operation state =
    case operation of
        RightShift ->
            rightShift state 

        LeftShift ->
            leftShift state

        Increment ->
            increment state

        Decrement ->
            decrement state

        Output ->
            output state


evaluate : Action -> State -> State
evaluate action state =
    case action of
        Command op ->
            { state | step = state.step + 1 } |> execute op

        Loop cmd ->
            if fromState state == 0 then
                state

            else
                let
                    result =
                        parse cmd []
                            |> fromList
                            |> foldl evaluate state
                in
                if fromState result == 0 then
                    result

                else
                    evaluate action result


run : String -> String
run code =
    let
        state =
            State 0 (repeat 30000 0) "" 0

        result =
            parse code []
                |> fromList
                |> foldl evaluate state
    in
    "Step: " ++ (fromInt result.step) ++ "\n" ++ result.output ++ "\n"
