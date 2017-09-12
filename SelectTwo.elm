module SelectTwo exposing (update, new, map, setSearch, basicSelectOptions, basicGroupSelectOptions)

{-| This library is the basic controls for your model's select2 object and some helper methods

#Basic Controls
@docs update, new, map, setSearch


# Helper Methods

@docs basicSelectOptions, basicGroupSelectOptions

-}

import SelectTwo.Types exposing (SelectTwoMsg(..), SelectTwo, SelectTwoDropdown, SelectTwoOption, AjaxParams, SelectTwoAjaxStuff, GroupSelectTwoOption, Model)
import SelectTwo.Private exposing (filterList, filterGroup, px, asTuple)
import Json.Decode as JD
import List.Extra
import Task
import Tuple
import Tuple3
import Dom
import Http
import Process
import Time
import Html exposing (text)


{-| used in your elm project's update as a way to control the select two boxes on screen it is used by

    yourUpdate : Msg -> Model -> ( Model, Cmd Msg )
    yourUpdate msg model =
        case msg of
            SelectTwo stmsg ->
                update stmsg model

-}
update : SelectTwoMsg msg -> Model b msg -> ( Model b msg, Cmd msg )
update msg model =
    case msg of
        SelectTwoTrigger p dd ->
            let
                newModel =
                    new p dd model
            in
                newModel ! [ Dom.focus ((dd.id_) ++ "--search") |> Task.attempt (STRes >> (dd.sender)), ajaxCmd identity False (map (\s -> { s | list = [] }) newModel) ]

        SelectTwoHovered hovered ->
            map (\s -> { s | hovered = hovered }) model ! []

        SelectTwoSelected msg ->
            { model | selectTwo = Nothing } ! [ msg |> Maybe.map send |> Maybe.withDefault Cmd.none ]

        SetSelectTwoSearch filter ->
            setSearch filter model
                ! [ model.selectTwo
                        |> Maybe.map .dropdown
                        |> Maybe.andThen (\dd -> delayedSend (dd.delay) ((dd.sender) (DelayedSelectTwoAjax filter)) |> Just)
                        |> Maybe.withDefault Cmd.none
                  ]

        DelayedSelectTwoAjax filter ->
            model
                ! [ model.selectTwo
                        |> Maybe.map .search
                        |> Maybe.andThen identity
                        |> Maybe.andThen
                            (\search ->
                                if search == filter then
                                    ajaxCmd (\p -> { p | page = 1, term = filter, more = False, loading = True }) True (map (\s -> { s | list = [] }) model) |> Just
                                else
                                    Nothing
                            )
                        |> Maybe.withDefault Cmd.none
                  ]

        SelectTwoAjax ( url, data, processResults, params ) reset (Ok str) ->
            let
                ( list, newParams ) =
                    processResults ( str, { params | loading = False } )

                tempList =
                    if reset then
                        list
                    else
                        (model.selectTwo |> Maybe.map .list |> Maybe.withDefault []) ++ list

                newList =
                    tempList
                        |> List.Extra.groupWhile (\x y -> Tuple.first x == Tuple.first y)
                        |> (List.map
                                (asTuple
                                    (List.head >> Maybe.map (Tuple.first) >> Maybe.withDefault "")
                                    (List.map (Tuple.second) >> List.concat)
                                )
                           )
            in
                map (\s -> { s | list = newList, ajaxStuff = Just ( url, data, processResults, newParams ) }) model ! []

        SelectTwoAjax ajaxStuff reset (Err str) ->
            map (\s -> { s | list = [], ajaxStuff = Just ajaxStuff }) model ! []

        ResultScroll { scrollHeight, scrollTop } ->
            checkScrollPage scrollTop scrollHeight model

        STMsg msg ->
            model ! [ send msg ]

        STRes a ->
            model ! []

        STNull ->
            model ! []


checkScrollPage : Int -> Int -> Model b msg -> ( Model b msg, Cmd msg )
checkScrollPage scrollTop scrollHeight model =
    let
        ajaxStuff =
            (model.selectTwo |> Maybe.map .ajaxStuff)
    in
        model.selectTwo
            |> Maybe.map .ajaxStuff
            |> Maybe.andThen identity
            |> Maybe.andThen (sendPageIncrement model scrollTop scrollHeight)
            |> Maybe.withDefault ( model, Cmd.none )


sendPageIncrement : Model b msg -> Int -> Int -> SelectTwoAjaxStuff msg -> Maybe ( Model b msg, Cmd msg )
sendPageIncrement model scrollTop scrollHeight ( url, data, processResults, params ) =
    if incrementPage scrollTop scrollHeight params then
        let
            newModel =
                map (\s -> { s | ajaxStuff = Just ( url, data, processResults, { params | loading = True, page = params.page + 1 } ) }) model
        in
            Just (newModel ! [ ajaxCmd identity False newModel ])
    else
        Nothing


incrementPage : Int -> Int -> AjaxParams -> Bool
incrementPage scrollTop scrollHeight params =
    let
        scrollPageHeight =
            (toFloat scrollHeight) / (toFloat params.page)

        scrollPageTop =
            (toFloat scrollTop) - (toFloat scrollHeight) + scrollPageHeight
    in
        scrollPageTop / scrollPageHeight > 0.7 && (not params.loading) && params.more


delayedSend : Float -> msg -> Cmd msg
delayedSend milli msg =
    Process.sleep (Time.millisecond * milli)
        |> Task.perform (\_ -> msg)


ajaxCmd : (AjaxParams -> AjaxParams) -> Bool -> Model b msg -> Cmd msg
ajaxCmd f reset model =
    case model.selectTwo of
        Just selectTwo ->
            let
                ajaxStuff =
                    if selectTwo.ajaxStuff == Nothing then
                        selectTwo.dropdown.ajaxStuff
                    else
                        selectTwo.ajaxStuff

                sender =
                    selectTwo.dropdown.sender
            in
                case ajaxStuff of
                    Just ( url, data, processResults, params ) ->
                        data ( url, f params )
                            |> Http.getString
                            |> Http.send ((SelectTwoAjax ( url, data, processResults, f params ) reset) >> sender)

                    Nothing ->
                        Cmd.none

        Nothing ->
            Cmd.none


{-| Create a new instance of the selectTwo record in your model
-}
new : List String -> SelectTwoDropdown msg -> Model b msg -> Model b msg
new parents dropdown model =
    { model | selectTwo = Just { dropdown = dropdown, hovered = Nothing, search = Nothing, parents = parents, list = [], ajaxStuff = dropdown.ajaxStuff } }


{-| modify selectTwo record in your model
-}
map : (SelectTwo msg -> SelectTwo msg) -> Model b msg -> Model b msg
map f model =
    let
        newSelectTwo =
            model.selectTwo |> Maybe.map f
    in
        { model | selectTwo = newSelectTwo }


{-| set the search parameter in your selectTwo record
-}
setSearch : String -> Model b msg -> Model b msg
setSearch filter =
    let
        search =
            if filter == "" then
                Nothing
            else
                Just filter
    in
        map (\s -> { s | search = search })


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


{-| turn a list of Tuples into a list of GroupSelectTwoOptions with one group. The first parameter is their shared trigger message, the
second parameter is the list of tuples which are formatted as (Key, "Display"), and the return can be used in the list option of the config

    [ ( Just "a", "a" )
    , ( Just "b", "b" )
    , ( Just "c", "c" )
    ]
        |> SelectTwo.basicSelectOptions Test

-}
basicSelectOptions : (a -> msg) -> List ( a, String ) -> List (GroupSelectTwoOption msg)
basicSelectOptions msg list =
    [ ( "", List.map (selectOption msg) list ) ]


{-| turn a list of Tuples3 into a list of GroupSelectTwoOptions with actual grouping. The first parameter is their shared trigger message, the
second parameter is the list of tuples which are formatted as (Key, "Display", "Group Name"), and the return can be used in the list option of the config

    [ ( Just "a", "a", "Group 1")
    , ( Just "b", "b", "Group 1")
    , ( Just "c", "c", "Group 2")
    ]
        |> SelectTwo.basicSelectOptions Test

-}
basicGroupSelectOptions : (a -> msg) -> List ( a, String, String ) -> List (GroupSelectTwoOption msg)
basicGroupSelectOptions msg list =
    list
        |> List.Extra.groupWhile (\( _, _, a ) ( _, _, b ) -> a == b)
        |> List.map (selectGroup msg)


selectGroup : (a -> msg) -> List ( a, String, String ) -> GroupSelectTwoOption msg
selectGroup msg list =
    ( list
        |> List.head
        |> Maybe.map Tuple3.third
        |> Maybe.withDefault ""
    , List.map (Tuple3.init >> (selectOption msg)) list
    )


selectOption : (a -> msg) -> ( a, String ) -> SelectTwoOption msg
selectOption msg ( val, txt ) =
    ( Just (msg val), text txt, txt, True )
