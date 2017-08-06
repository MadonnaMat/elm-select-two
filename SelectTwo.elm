module SelectTwo exposing (update, new, map, setFilter, filterList, filterGroup, location, ajaxLocation, basicSelectOptions, basicGroupSelectOptions, preventScrolling, widthGuess)

import SelectTwo.Types exposing (SelectTwoMsg(..), SelectTwo, SelectTwoDropdown, SelectTwoOption, AjaxParams, SelectTwoAjaxStuff, GroupSelectTwoOption, Model)
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
import SelectTwo.Helpers exposing (closest, px, asTuple)


update : SelectTwoMsg msg -> Model b msg -> ( Model b msg, Cmd msg )
update msg model =
    case msg of
        SelectTwoTrigger p dd ->
            let
                newModel =
                    new p dd model
            in
                newModel ! [ Dom.focus ((dd.id_) ++ "--search") |> Task.attempt (STRes >> (dd.sender)), ajaxCmd identity (map (\s -> { s | list = Just [] }) newModel) ]

        SelectTwoHovered hovered ->
            map (\s -> { s | hovered = hovered }) model ! []

        SelectTwoSelected msg ->
            { model | selectTwo = Nothing } ! [ msg |> Maybe.map send |> Maybe.withDefault Cmd.none ]

        SetSelectTwoSearch filter ->
            setFilter filter model
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
                                    ajaxCmd (\p -> { p | page = 1, term = filter, more = False, loading = True }) (map (\s -> { s | list = Just [] }) model) |> Just
                                else
                                    Nothing
                            )
                        |> Maybe.withDefault Cmd.none
                  ]

        SelectTwoAjax ( url, data, processResults, params ) (Ok str) ->
            let
                ( list, newParams ) =
                    processResults ( str, { params | loading = False } )

                newList =
                    ((model.selectTwo |> Maybe.andThen .list |> Maybe.withDefault []) ++ list)
                        |> List.Extra.groupWhile (\x y -> Tuple.first x == Tuple.first y)
                        |> (List.map
                                (asTuple
                                    (List.head >> Maybe.map (Tuple.first) >> Maybe.withDefault "")
                                    (List.map (Tuple.second) >> List.concat)
                                )
                           )
            in
                map (\s -> { s | list = Just newList, ajaxStuff = Just ( url, data, processResults, newParams ) }) model ! []

        SelectTwoAjax ajaxStuff (Err str) ->
            map (\s -> { s | list = Nothing, ajaxStuff = Just ajaxStuff }) model ! []

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
            Just (newModel ! [ ajaxCmd identity newModel ])
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


ajaxCmd : (AjaxParams -> AjaxParams) -> Model b msg -> Cmd msg
ajaxCmd f model =
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
                            |> Http.send ((SelectTwoAjax ( url, data, processResults, f params )) >> sender)

                    Nothing ->
                        Cmd.none

        Nothing ->
            Cmd.none


new : List String -> SelectTwoDropdown msg -> Model b msg -> Model b msg
new p dd model =
    { model | selectTwo = Just { dropdown = dd, hovered = Nothing, search = Nothing, parents = p, list = Nothing, ajaxStuff = Nothing } }


map : (SelectTwo msg -> SelectTwo msg) -> Model b msg -> Model b msg
map f model =
    let
        newSelectTwo =
            model.selectTwo |> Maybe.map f
    in
        { model | selectTwo = newSelectTwo }


setFilter : String -> Model b msg -> Model b msg
setFilter filter =
    let
        search =
            if filter == "" then
                Nothing
            else
                Just filter
    in
        map (\s -> { s | search = search })


filterGroup : Maybe String -> GroupSelectTwoOption a -> GroupSelectTwoOption a
filterGroup filter list =
    list |> Tuple.mapSecond (List.filter (filterList filter))


filterList : Maybe String -> SelectTwoOption a -> Bool
filterList filter ( _, _, text ) =
    filter
        |> Maybe.andThen ((String.toLower) >> (flip String.contains (text |> String.toLower)) >> Just)
        |> Maybe.withDefault True


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


parentSize : String -> List String -> JD.Decoder Float
parentSize dir oldParents =
    let
        uncons =
            oldParents |> List.Extra.uncons
    in
        case uncons of
            Just ( parent, parents ) ->
                JD.map2 (+)
                    (parentSize dir parents)
                    (JD.map2 (-)
                        ((closest parent << JD.field ("offset" ++ dir)) JD.float)
                        ((closest parent << JD.field ("scroll" ++ dir)) JD.float)
                    )

            Nothing ->
                JD.field "scrollTop" JD.float


location : String -> Float -> (SelectTwoMsg msg -> msg) -> List (SelectTwoOption msg) -> List (GroupSelectTwoOption msg) -> List String -> Bool -> JD.Decoder msg
location id_ delay sender defaults list parents showSearch =
    JD.map2 (,)
        (JD.map2 (,)
            (JD.map2 (+)
                (JD.at [ "offsetLeft" ] JD.float)
                (parentSize "Left" parents)
            )
            (JD.map2 (+)
                (JD.at [ "clientHeight" ] JD.float)
                (JD.map2 (+)
                    (JD.at [ "offsetTop" ] JD.float)
                    (parentSize "Top" parents)
                )
            )
        )
        (JD.at [ "offsetWidth" ] JD.float)
        |> JD.map
            (buildDropdown id_
                delay
                sender
                defaults
                list
                showSearch
                Nothing
            )
        |> JD.map ((SelectTwoTrigger parents) >> sender)
        |> (JD.field "target" << closest "select2")


ajaxLocation :
    String
    -> Float
    -> (SelectTwoMsg msg -> msg)
    -> List (SelectTwoOption msg)
    -> List String
    -> Bool
    -> String
    -> (( String, AjaxParams ) -> String)
    -> (( String, AjaxParams ) -> ( List (GroupSelectTwoOption msg), AjaxParams ))
    -> JD.Decoder msg
ajaxLocation id_ delay sender defaults parents showSearch url data processResults =
    JD.map2 (,)
        (JD.map2 (,)
            (JD.map2 (+)
                (JD.at [ "offsetLeft" ] JD.float)
                (parentSize "Left" parents)
            )
            (JD.map2 (+)
                (JD.at [ "clientHeight" ] JD.float)
                (JD.map2 (+)
                    (JD.at [ "offsetTop" ] JD.float)
                    (parentSize "Top" parents)
                )
            )
        )
        (JD.at [ "offsetWidth" ] JD.float)
        |> JD.map
            (buildDropdown id_
                delay
                sender
                defaults
                []
                showSearch
                (Just ( url, data, processResults, { page = 1, term = "", more = False, loading = True } ))
            )
        |> JD.map ((SelectTwoTrigger parents) >> sender)
        |> (JD.field "target" << closest "select2")


buildDropdown : String -> Float -> (SelectTwoMsg msg -> msg) -> List (SelectTwoOption msg) -> List (GroupSelectTwoOption msg) -> Bool -> Maybe (SelectTwoAjaxStuff msg) -> ( ( Float, Float ), Float ) -> SelectTwoDropdown msg
buildDropdown id_ delay sender defaults list showSearch ajaxStuff ( ( x, y ), width ) =
    { id_ = id_
    , sender = sender
    , defaults = defaults
    , list = list
    , showSearch = showSearch
    , x = x
    , y = y
    , width = width
    , ajaxStuff = ajaxStuff
    , delay = delay
    }


basicSelectOptions : (a -> msg) -> List ( a, String ) -> List (GroupSelectTwoOption msg)
basicSelectOptions msg list =
    [ ( "", List.map (selectOption msg) list ) ]


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
    ( Just (msg val), text txt, txt )


preventScrolling : String -> Model b msg -> List ( String, String )
preventScrolling name model =
    let
        prevent =
            model.selectTwo
                |> Maybe.map .parents
                |> Maybe.withDefault []
                |> List.member name
    in
        if prevent then
            [ ( "overflow", "hidden" ) ]
        else
            []


widthGuess : Float -> List (SelectTwoOption msg) -> String
widthGuess font list =
    (list
        |> List.map (\( _, _, x ) -> x)
        |> List.map (String.length)
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
    )
        * (font / 1.5)
        + 30
        |> px
