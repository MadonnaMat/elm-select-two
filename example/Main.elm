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
    }


init : ( Model, Cmd Msg )
init =
    { selectTwo = Nothing
    , test = Nothing
    }
        ! []


type Msg
    = Test (Maybe String)
    | SelectTwo (SelectTwoMsg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTwo stmsg ->
            SelectTwo.update stmsg model

        Test s ->
            { model | test = s } ! []


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
        , select2 SelectTwo
            { default = Test model.test
            , list = testList
            , parents = [ "parent" ]
            , clearMsg = Just (STMsg (Test Nothing))
            , showSearch = True
            , width = "300px"
            , placeholder = "Select Test"
            }
        , select2Dropdown model
        ]


testList : List (SelectTwoOption Msg)
testList =
    [ ( Just "a", "a" ), ( Just "b", "b" ), ( Just "c", "c" ) ] |> SelectTwo.basicSelectOptions Test
