module Main exposing (..)

import Browser
import Html exposing (Html, div, li, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onEnter)
import List exposing (append)
import Logic.Interpreter exposing (run)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { code : String
    , results : List String
    }


init : Model
init =
    { code = "", results = [] }



-- UPDATE


type Msg
    = Change String
    | Execute


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change value ->
            { model | code = value }

        Execute ->
            { code = ""
            , results = append model.results [ run model.code ]
            }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "background" "black"
        , style "padding" "24px"
        ]
        [ div
            [ style "color" "limegreen"
            ]
            [ text "Example: ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
            ]
        , resultList model.results
        , div
            [ style "color" "limegreen"
            ]
            [ text "Input:"
            ]
        , textarea
            [ style "width" "100%"
            , style "height" "240px"
            , style "resize" "none"
            , style "margin-bottom" "100%"
            , style "background" "transparent"
            , style "color" "limegreen"
            , style "border" "none"
            , style "border-radius" "0"
            , style "outline" "none"
            , autofocus True
            , value model.code
            , onInput Change
            , onEnter Execute
            ]
            []
        ]


resultList : List String -> Html msg
resultList list =
    ul
        [ style "color" "limegreen"
        , style "padding" "0px"
        , style "white-space" "pre"
        ]
        (List.map
            (\result ->
                li
                    [ style "list-style-type" "none" ]
                    [ text result ]
            )
            list
        )
