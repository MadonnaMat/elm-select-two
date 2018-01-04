module SelectTwo exposing (update, new, map, setSearch, basicSelectOptions, basicGroupSelectOptions, send, setLoading, setList)

{-| This library is the basic controls for your model's select2 object and some helper methods

#Basic Controls
@docs update, new, map, setSearch, send, setLoading, setList


# Helper Methods

@docs basicSelectOptions, basicGroupSelectOptions

-}

import SelectTwo.Types exposing (SelectTwoMsg(..), SelectTwo, SelectTwoDropdown, SelectTwoOption, AjaxParams, SelectTwoAjaxStuff, GroupSelectTwoOption, Model)
import SelectTwo.Private exposing (filterList, filterGroup, px, asTuple, defaultParams)
import Json.Decode as JD
import List.Extra
import Task
import Tuple
import Tuple3
import Dom
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
update : (SelectTwoMsg msg -> msg) -> SelectTwoMsg msg -> Model b msg -> ( Model b msg, Cmd msg )
update sender msg model =
    case msg of
        SelectTwoTrigger p dd ->
            let
                newModel =
                    new p dd model
            in
                newModel ! [ Dom.focus ((dd.id_) ++ "--search") |> Task.attempt (STRes >> (sender)), ajaxCmd sender (newModel.selectTwo |> Maybe.andThen .ajaxParams |> Maybe.withDefault (defaultParams "")) False (map (\s -> { s | list = [] }) newModel) ]

        SelectTwoHovered hovered ->
            map (\s -> { s | hovered = hovered }) model ! []

        SelectTwoSelected msg ->
            { model | selectTwo = Nothing } ! [ msg |> Maybe.map send |> Maybe.withDefault Cmd.none ]

        SetSelectTwoSearch filter ->
            setSearch filter model
                ! [ model.selectTwo
                        |> Maybe.map .dropdown
                        |> Maybe.andThen (\dd -> delayedSend (dd.ajax |> Maybe.map .delay |> Maybe.withDefault 0) ((sender) (DelayedSelectTwoAjax filter)) |> Just)
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
                                    ajaxCmd sender (defaultParams filter) True (map (\s -> { s | list = [] }) model) |> Just
                                else
                                    Nothing
                            )
                        |> Maybe.withDefault Cmd.none
                  ]

        SelectTwoAjax ajaxParams reset list ->
            let
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
                map (\s -> { s | list = newList, ajaxParams = Just ajaxParams }) model ! []

        ResultScroll { scrollHeight, scrollTop } ->
            checkScrollPage scrollTop scrollHeight sender model

        STMsg msg ->
            model ! [ send msg ]

        STRes a ->
            model ! []

        STNull ->
            model ! []

        SentAjax _ _ _ ->
            model ! []


checkScrollPage : Int -> Int -> (SelectTwoMsg msg -> msg) -> Model b msg -> ( Model b msg, Cmd msg )
checkScrollPage scrollTop scrollHeight sender model =
    model.selectTwo
        |> Maybe.andThen .ajaxParams
        |> Maybe.andThen (sendPageIncrement sender model scrollTop scrollHeight)
        |> Maybe.withDefault ( model, Cmd.none )


sendPageIncrement : (SelectTwoMsg msg -> msg) -> Model b msg -> Int -> Int -> AjaxParams -> Maybe ( Model b msg, Cmd msg )
sendPageIncrement sender model scrollTop scrollHeight ajaxParams =
    if incrementPage scrollTop scrollHeight ajaxParams then
        let
            newModel =
                map (\s -> { s | ajaxParams = Just { ajaxParams | page = ajaxParams.page + 1 } }) model
        in
            Just (newModel ! [ ajaxCmd sender (newModel.selectTwo |> Maybe.andThen .ajaxParams |> Maybe.withDefault (defaultParams "")) False newModel ])
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


ajaxCmd : (SelectTwoMsg msg -> msg) -> AjaxParams -> Bool -> Model b msg -> Cmd msg
ajaxCmd sender ajaxParams reset model =
    model.selectTwo
        |> Maybe.andThen .ajax
        |> Maybe.map .ajaxId
        |> Maybe.map
            (\ajaxId ->
                send (SentAjax ajaxId ajaxParams reset |> sender)
            )
        |> Maybe.withDefault Cmd.none


{-| Create a new instance of the selectTwo record in your model
-}
new : List String -> SelectTwoDropdown msg -> Model b msg -> Model b msg
new parents dropdown model =
    { model | selectTwo = Just { dropdown = dropdown, hovered = Nothing, search = Nothing, parents = parents, list = [], ajaxParams = Just <| defaultParams "", ajax = dropdown.ajax } }


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


{-| a
-}
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


{-| a
-}
setLoading : AjaxParams -> Bool -> Model b msg -> Model b msg
setLoading ajaxParams reset model =
    let
        list =
            model.selectTwo |> Maybe.map .list |> Maybe.withDefault []
    in
        map
            (\s ->
                { s
                    | list =
                        if reset then
                            []
                        else
                            list
                    , ajaxParams = Just { ajaxParams | loading = True }
                }
            )
            model


{-| a
-}
setList : List (GroupSelectTwoOption msg) -> AjaxParams -> Model b msg -> Model b msg
setList list ajaxParams model =
    let
        tempList =
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
        map (\s -> { s | list = newList, ajaxParams = Just { ajaxParams | loading = False } }) model
