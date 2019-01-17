module SelectTwo exposing
    ( update, new, map, setSearch
    , setLoading, setList
    , basicSelectOptions, basicGroupSelectOptions, defaultsFromList, send
    )

{-| This library is the basic controls for your model's select2 object and some helper methods

#Basic Controls

@docs update, new, map, setSearch


# Ajax Methods

@docs setLoading, setList


# Helper Methods

@docs basicSelectOptions, basicGroupSelectOptions, defaultsFromList, send

-}

import Browser.Dom as Dom
import Html exposing (text)
import Json.Decode as JD
import List.Extra
import Process
import SelectTwo.Private exposing (asTuple, defaultParams, filterGroup, filterList, groupWhile, px, tuple3First, tuple3Init, tuple3Third, uncurry3)
import SelectTwo.Types exposing (AjaxParams, GroupSelectTwoOption, Model, SelectTwo, SelectTwoDropdown, SelectTwoMsg(..), SelectTwoOption)
import Task
import Time
import Tuple


{-| used in your elm project's update as a way to control the select two boxes on screen it is used by

    yourUpdate : Msg -> Model -> ( Model, Cmd Msg )
    yourUpdate msg model =
        let
            ajaxSend =
                Just
                    (\id_ params reset ->
                        case id_ of
                            "test-4" ->
                                ( model
                                , SelectTwo.send <| TestAjax params reset
                                )

                            _ ->
                                ( model
                                , Cmd.none
                                )
                    )
        in
        SelectTwo.update SelectTwo stmsg ajaxSend model

-}
update : (SelectTwoMsg msg -> msg) -> SelectTwoMsg msg -> Maybe (String -> AjaxParams -> Bool -> ( Model b msg, Cmd msg )) -> Model b msg -> ( Model b msg, Cmd msg )
update sender msg maybeAjax model =
    case msg of
        SelectTwoTrigger dd ->
            let
                newModel =
                    new dd model
            in
            ( newModel
            , Dom.getElement dd.id_ |> Task.attempt (SelectTwoOpen dd >> sender)
            )

        SelectTwoOpen dd (Ok elem) ->
            let
                newModel =
                    updateDropdown elem model
            in
            ( newModel, Cmd.batch [ Dom.focus (dd.id_ ++ "--search") |> Task.attempt (STRes >> sender), ajaxCmd sender (newModel.selectTwo |> Maybe.andThen .ajaxParams |> Maybe.withDefault (defaultParams "")) False (map (\s -> { s | list = [] }) newModel) ] )

        SelectTwoOpen dd (Err _) ->
            ( { model | selectTwo = Nothing }
            , Cmd.none
            )

        SelectTwoHovered hovered ->
            ( map (\s -> { s | hovered = hovered }) model
            , Cmd.none
            )

        SelectTwoSelected mg ->
            ( { model | selectTwo = Nothing }
            , mg |> Maybe.map send |> Maybe.withDefault Cmd.none
            )

        SetSelectTwoSearch filter ->
            ( setSearch filter model
            , model.selectTwo
                |> Maybe.map .dropdown
                |> Maybe.andThen (\dd -> delayedSend dd.delay (sender (DelayedSelectTwoAjax filter)) |> Just)
                |> Maybe.withDefault Cmd.none
            )

        DelayedSelectTwoAjax filter ->
            ( model
            , model.selectTwo
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
            )

        ResultScroll { scrollHeight, scrollTop } ->
            checkScrollPage scrollTop scrollHeight sender model

        STMsg mg ->
            ( model
            , send mg
            )

        STRes a ->
            ( model
            , Cmd.none
            )

        STNull ->
            ( model
            , Cmd.none
            )

        SentAjax id_ params reset ->
            maybeAjax
                |> Maybe.map (\a -> uncurry3 a ( id_, params, reset ))
                |> Maybe.withDefault ( model, Cmd.none )


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
        Just
            ( newModel
            , ajaxCmd sender (newModel.selectTwo |> Maybe.andThen .ajaxParams |> Maybe.withDefault (defaultParams "")) False newModel
            )

    else
        Nothing


incrementPage : Int -> Int -> AjaxParams -> Bool
incrementPage scrollTop scrollHeight params =
    let
        scrollPageHeight =
            toFloat scrollHeight / toFloat params.page

        scrollPageTop =
            toFloat scrollTop - toFloat scrollHeight + scrollPageHeight
    in
    scrollPageTop / scrollPageHeight > 0.7 && not params.loading && params.more


delayedSend : Float -> msg -> Cmd msg
delayedSend milli msg =
    Process.sleep (1000 * milli)
        |> Task.perform (\_ -> msg)


ajaxCmd : (SelectTwoMsg msg -> msg) -> AjaxParams -> Bool -> Model b msg -> Cmd msg
ajaxCmd sender ajaxParams reset model =
    model.selectTwo
        |> Maybe.andThen
            (\st ->
                if st.ajax then
                    Just st.id_

                else
                    Nothing
            )
        |> Maybe.map
            (\id_ ->
                send (SentAjax id_ ajaxParams reset |> sender)
            )
        |> Maybe.withDefault Cmd.none


{-| Create a new instance of the selectTwo record in your model
-}
new : SelectTwoDropdown msg -> Model b msg -> Model b msg
new dropdown model =
    { model | selectTwo = Just { dropdown = dropdown, hovered = Nothing, search = Nothing, list = [], ajaxParams = Just <| defaultParams "", ajax = dropdown.ajax, id_ = dropdown.id_ } }


updateDropdown : Dom.Element -> Model b msg -> Model b msg
updateDropdown elem model =
    let
        selectTwo =
            model.selectTwo
                |> Maybe.map
                    (\st ->
                        let
                            dropdown =
                                st.dropdown

                            newDropdown =
                                { dropdown | x = elem.element.x, y = elem.element.y + elem.element.height, width = elem.element.width }
                        in
                        { st | dropdown = newDropdown }
                    )
    in
    { model | selectTwo = selectTwo }


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


{-| Quick helper method for sending a message and running through your update function agian

    update msg model =
        case msg of
            Test1 ->
                ( model
                , SelectTwo.send Test2
                )

            Test2 ->
                ( model
                , Cmd.none
                )

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

    [ ( Just "a", "a", "Group 1" )
    , ( Just "b", "b", "Group 1" )
    , ( Just "c", "c", "Group 2" )
    ]
        |> SelectTwo.basicSelectOptions Test

-}
basicGroupSelectOptions : (a -> msg) -> List ( a, String, String ) -> List (GroupSelectTwoOption msg)
basicGroupSelectOptions msg list =
    list
        |> groupWhile (\( _, _, a ) ( _, _, b ) -> a == b)
        |> List.map (selectGroup msg)


selectGroup : (a -> msg) -> List ( a, String, String ) -> GroupSelectTwoOption msg
selectGroup msg list =
    ( list
        |> List.head
        |> Maybe.map tuple3Third
        |> Maybe.withDefault ""
    , List.map (tuple3Init >> selectOption msg) list
    )


selectOption : (a -> msg) -> ( a, String ) -> SelectTwoOption msg
selectOption msg ( val, txt ) =
    ( Just (msg val), txt, True )


{-| To be used in your ajax functions to set loading state before sending

        TestAjax params reset ->
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
                SelectTwo.setLoading params reset model ! [ sendAjax buildUrl (TestRes params) ]

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


{-| Set the list from the return of your ajax command

        TestRes params (Ok str) ->
            let
                ( list, newParams ) =
                    processResult Test str params
            in
                SelectTwo.setList list newParams model ! []

-}
setList : List (GroupSelectTwoOption msg) -> AjaxParams -> Model b msg -> Model b msg
setList list ajaxParams model =
    let
        tempList =
            (model.selectTwo |> Maybe.map .list |> Maybe.withDefault []) ++ list

        newList =
            tempList
                |> groupWhile (\x y -> Tuple.first x == Tuple.first y)
                |> List.map
                    (asTuple
                        (List.head >> Maybe.map Tuple.first >> Maybe.withDefault "")
                        (List.map Tuple.second >> List.concat)
                    )
    in
    map (\s -> { s | list = newList, ajaxParams = Just { ajaxParams | loading = False } }) model


{-| Quick helper way of getting defaults list from a GroupSelectTwoOption list
-}
defaultsFromList : List msg -> List (GroupSelectTwoOption msg) -> List (SelectTwoOption msg)
defaultsFromList defaults list =
    list
        |> List.concatMap (\( _, l ) -> l)
        |> List.filter
            (\l ->
                l
                    |> tuple3First
                    |> Maybe.map (\a -> List.member a defaults)
                    |> Maybe.withDefault False
            )
