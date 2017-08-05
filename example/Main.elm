module Main exposing (..)

import Html exposing (Html, program, text, div)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SelectTwo exposing (..)
import SelectTwo.Html exposing (..)
import SelectTwoTypes exposing (..)
import Task


type alias Model =
    { selectTwo : Maybe (SelectTwo Msg)
    , test : Maybe String
    , test2 : Maybe String
    , test3 : List (Maybe String)
    }


init : ( Model, Cmd Msg )
init =
    { selectTwo = Nothing
    , test = Nothing
    , test2 = Nothing
    , test3 = []
    }
        ! []


type Msg
    = Test (Maybe String)
    | Test2 (Maybe String)
    | Test3 (Maybe String)
    | SelectTwo (SelectTwoMsg Msg)
    | Test3Clear Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTwo stmsg ->
            SelectTwo.update stmsg model

        Test s ->
            { model | test = s } ! []

        Test2 s ->
            { model | test2 = s } ! []

        Test3 s ->
            { model | test3 = s :: model.test3 } ! []

        Test3Clear (Test3 s) ->
            { model | test3 = model.test3 |> (List.filter ((/=) s)) } ! []

        Test3Clear _ ->
            model ! []


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ class "parent"
        , style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            ]
        , select2Close SelectTwo
        ]
        [ select2Css
        , div []
            [ select2 SelectTwo
                { default = Test model.test
                , list = testList Test
                , id_ = "test-1"
                , parents = [ "parent" ]
                , clearMsg = Just (Test Nothing)
                , showSearch = True
                , width = "300px"
                , placeholder = "Select Test"
                }
            ]
        , div []
            [ select2 SelectTwo
                { default = Test2 model.test2
                , list = testList2 Test2
                , parents = [ "parent" ]
                , id_ = "test-2"
                , clearMsg = Just (Test2 Nothing)
                , showSearch = True
                , width = "300px"
                , placeholder = "Select Test"
                }
            ]
        , div []
            [ select2Multiple SelectTwo
                { defaults = model.test3 |> List.map Test3
                , list = testList Test3
                , id_ = "test-3"
                , parents = [ "parent" ]
                , clearMsg = Test3Clear
                , width = "300px"
                , placeholder = "Select Test"
                }
            ]
        , select2Dropdown model
        ]


testList : (Maybe String -> Msg) -> List ( String, List (SelectTwoOption Msg) )
testList msg =
    [ ( Just "a", "a" )
    , ( Just "b", "b" )
    , ( Just "c", "c" )
    , ( Just "d", "Decons Chons" )
    , ( Just "e", "Fangroana" )
    , ( Just "f", "Ender's Game" )
    ]
        |> SelectTwo.basicSelectOptions msg


testList2 : (Maybe String -> Msg) -> List ( String, List (SelectTwoOption Msg) )
testList2 msg =
    [ ( Just "a", "a", "a" ), ( Just "b", "b", "a" ), ( Just "c", "c", "b" ) ] |> SelectTwo.basicGroupSelectOptions msg
