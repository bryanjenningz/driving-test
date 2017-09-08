module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


scenarios : List Scenario
scenarios =
    [ Scenario
        (Directions (Just 1) (Just 0) (Just 1) (Just 1))
        "Which car has the right of way?"
        [ "The top car", "The bottom car" ]
        1
    ]


type alias Model =
    { scenarioIndex : Int
    , answerIndexes : List Int
    }


type alias Scenario =
    { directions : Directions
    , question : String
    , answers : List String
    , correctIndex : Int
    }


type alias Directions =
    { up : Maybe Int
    , down : Maybe Int
    , left : Maybe Int
    , right : Maybe Int
    }


type Msg
    = ChooseAnswer Int
    | RetakeTest


init : ( Model, Cmd Msg )
init =
    ( Model 0 [], Cmd.none )


view : Model -> Html Msg
view model =
    let
        maybeScenario =
            scenarios
                |> List.drop model.scenarioIndex
                |> List.head
    in
        case maybeScenario of
            Just scenario ->
                div []
                    [ div [ class "roads" ]
                        [ viewRoads, viewCars scenario.directions ]
                    , viewQuestion scenario.question
                    , viewAnswers scenario.answers
                    ]

            Nothing ->
                viewResults model


viewRoads : Html msg
viewRoads =
    div []
        [ div [ class "x-road" ] []
        , div [ class "x-road-lines" ] []
        , div [ class "y-road" ] []
        , div [ class "y-road-lines" ] []
        , div [ class "intersection" ] []
        ]


viewCars : Directions -> Html msg
viewCars directions =
    div []
        [ viewCar "up" directions.up
        , viewCar "down" directions.down
        , viewCar "left" directions.left
        , viewCar "right" directions.right
        ]


viewCar : String -> Maybe Int -> Html msg
viewCar direction maybeDelay =
    case maybeDelay of
        Nothing ->
            div [] []

        Just delay ->
            div
                [ class ("car " ++ direction)
                , style [ ( "animation-delay", toString delay ++ "ms" ) ]
                ]
                []


viewQuestion : String -> Html msg
viewQuestion question =
    div [ class "question" ] [ text question ]


viewAnswers : List String -> Html Msg
viewAnswers answers =
    div [] (List.indexedMap viewAnswer answers)


viewAnswer : Int -> String -> Html Msg
viewAnswer index answer =
    div [ onClick (ChooseAnswer index) ] [ text answer ]


viewResults : Model -> Html Msg
viewResults model =
    let
        correctCount : Int
        correctCount =
            List.map2
                (==)
                model.answerIndexes
                (List.map .correctIndex scenarios)
                |> List.filter identity
                |> List.length

        totalCount : Int
        totalCount =
            List.length model.answerIndexes
    in
        div []
            [ text ("Your score is: " ++ toString correctCount ++ " / " ++ toString totalCount)
            , div [] [ button [ onClick RetakeTest ] [ text "Retake Test" ] ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseAnswer index ->
            ( { model
                | answerIndexes = model.answerIndexes ++ [ index ]
                , scenarioIndex = model.scenarioIndex + 1
              }
            , Cmd.none
            )

        RetakeTest ->
            ( { model | answerIndexes = [], scenarioIndex = 0 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
