module SelectTwo exposing (update, new, map, setFilter, filterList, filterGroup, location, basicSelectOptions, basicGroupSelectOptions, preventScrolling, widthGuess)

import SelectTwoTypes exposing (SelectTwoMsg(..), SelectTwo, SelectTwoDropdown, SelectTwoOption, GroupSelectTwoOption)
import Json.Decode as JD
import List.Extra
import Task
import Tuple
import Tuple3
import Dom
import Helpers exposing (closest, px)


update : SelectTwoMsg msg -> { b | selectTwo : Maybe (SelectTwo msg) } -> ( { b | selectTwo : Maybe (SelectTwo msg) }, Cmd msg )
update msg model =
    case msg of
        SelectTwoTrigger p dd ->
            let
                sender =
                    dd
                        |> dropdownSender
            in
                new p dd model ! [ Dom.focus ((dd |> dropdownId) ++ "--search") |> Task.attempt (STRes >> sender) ]

        SelectTwoHovered hovered ->
            map (\s -> { s | hovered = hovered }) model ! []

        SelectTwoSelected msg ->
            { model | selectTwo = Nothing } ! [ msg |> Maybe.map send |> Maybe.withDefault Cmd.none ]

        SetSelectTwoSearch filter ->
            setFilter filter model ! []

        STMsg msg ->
            model ! [ send msg ]

        STRes a ->
            model ! []

        STNull ->
            model ! []


new : List String -> SelectTwoDropdown a -> { b | selectTwo : Maybe (SelectTwo a) } -> { b | selectTwo : Maybe (SelectTwo a) }
new p dd model =
    { model | selectTwo = Just { dropdown = dd, hovered = Nothing, search = Nothing, parents = p } }


map : (SelectTwo a -> SelectTwo a) -> { b | selectTwo : Maybe (SelectTwo a) } -> { b | selectTwo : Maybe (SelectTwo a) }
map f model =
    let
        newSelectTwo =
            model.selectTwo |> Maybe.map f
    in
        { model | selectTwo = newSelectTwo }


setFilter : String -> { b | selectTwo : Maybe (SelectTwo a) } -> { b | selectTwo : Maybe (SelectTwo a) }
setFilter filter =
    let
        search =
            if filter == "" then
                Nothing
            else
                Just filter
    in
        map (\s -> { s | search = search })


dropdownId : SelectTwoDropdown a -> String
dropdownId ( id_, _, _, _, _, _, _, _ ) =
    id_


dropdownSender : SelectTwoDropdown a -> SelectTwoMsg a -> a
dropdownSender ( _, sender, _, _, _, _, _, _ ) =
    sender


filterGroup : Maybe String -> GroupSelectTwoOption a -> GroupSelectTwoOption a
filterGroup filter list =
    list |> Tuple.mapSecond (List.filter (filterList filter))


filterList : Maybe String -> SelectTwoOption a -> Bool
filterList filter ( _, text ) =
    case filter of
        Just f ->
            String.contains (f |> String.toLower) (text |> String.toLower)

        Nothing ->
            True


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


location : String -> (SelectTwoMsg msg -> msg) -> List msg -> List (GroupSelectTwoOption msg) -> List String -> Bool -> JD.Decoder msg
location id_ sender defaults list parents showSearch =
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
        |> JD.map (buildDropdown id_ sender defaults list showSearch)
        |> JD.map ((SelectTwoTrigger parents) >> sender)
        |> (JD.field "target" << closest "select2")


buildDropdown : String -> (SelectTwoMsg msg -> msg) -> List msg -> List (GroupSelectTwoOption msg) -> Bool -> ( ( Float, Float ), Float ) -> SelectTwoDropdown msg
buildDropdown id_ sender defaults list showSearch ( ( x, y ), width ) =
    ( id_, sender, defaults, list, showSearch, x, y, width )


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
    let
        group =
            list |> List.head |> Maybe.map Tuple3.third |> Maybe.withDefault ""
    in
        ( group, List.map (\( a, b, _ ) -> selectOption msg ( a, b )) list )


selectOption : (a -> msg) -> ( a, String ) -> SelectTwoOption msg
selectOption msg ( val, txt ) =
    ( Just (msg val), txt )


preventScrolling : String -> Maybe (SelectTwo msg) -> List ( String, String )
preventScrolling name selectTwo =
    let
        prevent =
            selectTwo
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
        |> List.map (\( _, x ) -> x)
        |> List.map (String.length)
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
    )
        * (font / 1.5)
        + 30
        |> px
