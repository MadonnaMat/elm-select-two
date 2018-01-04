module Main exposing (..)

import Html exposing (Html, program, text, div, span, p, h1)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SelectTwo exposing (..)
import SelectTwo.Html exposing (..)
import SelectTwo.Types exposing (..)
import Task
import Tuple3
import Json.Decode as JD
import Http


type alias Model =
    { selectTwo : Maybe (SelectTwo Msg)
    , test : Maybe String
    , test2 : Maybe String
    , test3 : List (Maybe String)
    , test4 : Maybe { id : Int, name : String }
    , test5 : List { id : Int, name : String }
    }


init : ( Model, Cmd Msg )
init =
    { selectTwo = Nothing
    , test = Nothing
    , test2 = Nothing
    , test3 = []
    , test4 = Nothing
    , test5 = []
    }
        ! []


type Msg
    = Test (Maybe String)
    | Test2 (Maybe String)
    | Test3 (Maybe String)
    | Test4 (Maybe { id : Int, name : String })
    | Test5 (Maybe { id : Int, name : String })
    | SelectTwo (SelectTwoMsg Msg)
    | Test3Clear (Maybe Msg)
    | Test5Clear (Maybe Msg)
    | Test4Ajax AjaxParams Bool
    | Test4Res AjaxParams (Result Http.Error String)
    | Test5Ajax AjaxParams Bool
    | Test5Res AjaxParams (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTwo stmsg ->
            case stmsg of
                SentAjax id_ params reset ->
                    case id_ of
                        "test-4" ->
                            model ! [ SelectTwo.send <| Test4Ajax params reset ]

                        "test-5" ->
                            model ! [ SelectTwo.send <| Test5Ajax params reset ]

                        _ ->
                            SelectTwo.update (SelectTwo) stmsg model

                _ ->
                    SelectTwo.update (SelectTwo) stmsg model

        Test s ->
            { model | test = s } ! []

        Test2 s ->
            { model | test2 = s } ! []

        Test3 s ->
            { model | test3 = s :: model.test3 } ! []

        Test4 s ->
            { model | test4 = s } ! []

        Test5 (Just s) ->
            { model | test5 = s :: model.test5 } ! []

        Test5 _ ->
            model ! []

        Test3Clear (Just (Test3 s)) ->
            { model | test3 = model.test3 |> (List.filter ((/=) s)) } ! []

        Test5Clear (Just (Test5 (Just s))) ->
            { model | test5 = model.test5 |> (List.filter ((/=) s)) } ! []

        Test3Clear _ ->
            model ! []

        Test5Clear _ ->
            model ! []

        Test4Ajax params reset ->
            let
                url =
                    "//api.github.com/search/repositories"

                buildUrl =
                    let
                        term =
                            if params.term == "" then
                                "test"
                            else
                                params.term
                    in
                        (url ++ "?q=" ++ term ++ "&page=" ++ (toString params.page))
            in
                SelectTwo.setLoading params reset model ! [ sendAjax buildUrl (Test4Res params) ]

        Test4Res params (Ok str) ->
            let
                ( list, newParams ) =
                    processResult Test4 str params
            in
                SelectTwo.setList list newParams model ! []

        Test5Ajax params reset ->
            let
                url =
                    "//api.github.com/search/repositories"

                buildUrl =
                    let
                        term =
                            if params.term == "" then
                                "test"
                            else
                                params.term
                    in
                        (url ++ "?q=" ++ term ++ "&page=" ++ (toString params.page))
            in
                SelectTwo.setLoading params reset model ! [ sendAjax buildUrl (Test5Res params) ]

        Test5Res params (Ok str) ->
            let
                ( list, newParams ) =
                    processResult Test5 str params
            in
                SelectTwo.setList list newParams model ! []


sendAjax : String -> (Result Http.Error String -> Msg) -> Cmd Msg
sendAjax url msg =
    Http.getString url
        |> Http.send msg


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
        [ style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "padding", "30px" )
            , ( "font-size", "16px" )
            ]
        , select2Close SelectTwo
        ]
        [ select2Css
        , h1 [] [ text "Examples of Elm Select2" ]
        , p []
            [ text "Single Group, Single Select, Width Guess'd"
            , div []
                [ select2 SelectTwo
                    { defaults = SelectTwo.defaultsFromList [ Test model.test ] <| testList Test
                    , id_ = "test-1"
                    , list = testList Test
                    , parents = []
                    , clearMsg = Just (\_ -> Test Nothing)
                    , width = widthGuess 16 (testList Test)
                    , placeholder = "Select Test"
                    , disabled = model.test2 == Just "a"
                    , showSearch = True
                    , multiSelect = False
                    , ajax = False
                    , delay = 0
                    , noResultsMessage = Nothing
                    }
                ]
            ]
        , p [ class "p1", style [ ( "position", "relative" ) ] ]
            [ text "Multiple Groups, Single Select, Relative Parent"
            , div []
                [ select2 SelectTwo
                    { defaults = SelectTwo.defaultsFromList [ Test2 model.test2 ] <| testList2 Test2
                    , id_ = "test-2"
                    , list = testList2 Test2
                    , parents = [ "p1" ]
                    , clearMsg = Just (\_ -> Test2 Nothing)
                    , width = "300px"
                    , placeholder = "Select Test"
                    , disabled = False
                    , showSearch = True
                    , multiSelect = False
                    , ajax = False
                    , delay = 0
                    , noResultsMessage = Nothing
                    }
                ]
            ]
        , p
            [ class "p2"
            , style
                [ ( "position", "absolute" )
                , ( "top", "6em" )
                , ( "right", "50px" )
                ]
            ]
            [ text "Single Group, Multi-Select, Custom Rows, Position Absolute"
            , div []
                [ select2 SelectTwo
                    { defaults = SelectTwo.defaultsFromList (model.test3 |> List.map Test3) <| testList3 Test3
                    , id_ = "test-3"
                    , list = testList3 Test3
                    , parents = [ "p2" ]
                    , clearMsg = Just Test3Clear
                    , width = "300px"
                    , placeholder = "Select Test"
                    , disabled = model.test2 == Just "a"
                    , showSearch = False
                    , multiSelect = True
                    , ajax = False
                    , delay = 0
                    , noResultsMessage = Nothing
                    }
                ]
            ]
        , p []
            [ text "Ajax, Single Select"
            , div []
                [ select2 SelectTwo
                    { defaults = [ model.test4 |> Maybe.map (\t -> ( Just (Test4 (Just t)), t.name, True )) |> Maybe.withDefault ( Nothing, "", True ) ]
                    , ajax = True
                    , delay = 300
                    , id_ = "test-4"
                    , parents = []
                    , clearMsg = Just (\_ -> Test4 Nothing)
                    , showSearch = True
                    , width = "300px"
                    , placeholder = "Select Test"
                    , list = []
                    , multiSelect = False
                    , disabled = model.test2 == Just "a"
                    , noResultsMessage = Just "YOU GET NOTHING! YOU LOSE GOODDAY SIR!"
                    }
                ]
            ]
        , p []
            [ text "Ajax, Multi Select"
            , div []
                [ select2 SelectTwo
                    { defaults = model.test5 |> List.map (\t -> ( Just (Test5 (Just t)), t.name, True ))
                    , ajax = True
                    , delay = 300
                    , id_ = "test-5"
                    , parents = []
                    , clearMsg = Just Test5Clear
                    , showSearch = True
                    , width = "300px"
                    , placeholder = "Select Test"
                    , list = []
                    , multiSelect = True
                    , disabled = model.test2 == Just "a"
                    , noResultsMessage = Nothing
                    }
                ]
            ]
        , select2Dropdown SelectTwo (Just customHtml) model
        ]


processResult : (Maybe { id : Int, name : String } -> Msg) -> String -> AjaxParams -> ( List (GroupSelectTwoOption Msg), AjaxParams )
processResult msg string params =
    (JD.decodeString
        ((JD.map2 (,)
            (JD.at [ "items" ] (JD.list itemsDecoder))
            (JD.field "total_count" JD.int)
         )
            |> JD.map
                (\( items, total_count ) ->
                    ( items |> List.map (\i -> ( Just i, i.name )) |> SelectTwo.basicSelectOptions msg
                    , { params | more = (params.page * 30 < total_count) }
                    )
                )
        )
        string
    )
        |> Result.toMaybe
        |> Maybe.withDefault ( [], params )


type alias Item =
    { id : Int, name : String }


itemsDecoder : JD.Decoder Item
itemsDecoder =
    JD.map2 Item
        (JD.field ("id") JD.int)
        (JD.field ("name") JD.string)


testList : (Maybe String -> Msg) -> List ( String, List (SelectTwoOption Msg) )
testList msg =
    [ ( Just "a", "Harry Potter" )
    , ( Just "b", "Ender's Game" )
    , ( Just "c", "Dune" )
    , ( Just "d", "Foundation" )
    , ( Just "e", "Jurassic Park" )
    ]
        |> SelectTwo.basicSelectOptions msg


testList2 : (Maybe String -> Msg) -> List ( String, List (SelectTwoOption Msg) )
testList2 msg =
    [ ( Just "a", "Disable All Other Boxes", "Group 1" ), ( Just "b", "b", "Group 1" ), ( Just "c", "c", "Group 2" ) ] |> SelectTwo.basicGroupSelectOptions msg


testList3 : (Maybe String -> msg) -> List (GroupSelectTwoOption msg)
testList3 msg =
    ( ""
    , [ ( Just "a", "Harry Potter" )
      , ( Just "b", "Ender's Game" )
      , ( Just "c", "Dune" )
      , ( Just "d", "Foundation" )
      , ( Just "e", "Jurassic Park" )
      ]
        |> List.map
            (\( a, b ) ->
                ( Just (msg a)
                , b
                , True
                )
            )
    )
        :: []


customHtml : SelectTwoOption Msg -> Maybe (Html Msg)
customHtml ( msg, txt, sel ) =
    case msg of
        Just (Test3 _) ->
            Just <|
                span
                    [ style
                        [ ( "width", "100%" )
                        , ( "text-align", "center" )
                        , ( "display", "inline-block" )
                        ]
                    ]
                    [ text txt
                    ]

        _ ->
            Nothing
