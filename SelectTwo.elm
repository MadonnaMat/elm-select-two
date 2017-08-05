module SelectTwo exposing (update, new, map, setFilter, filterList, location, basicSelectOptions, preventScrolling, widthGuess)

import SelectTwoTypes exposing (SelectTwoMsg(..), SelectTwo, SelectTwoDropdown, SelectTwoOption)
import Json.Decode as JD
import List.Extra
import Task
import Helpers exposing (closest, px)


update : SelectTwoMsg msg -> { b | selectTwo : Maybe (SelectTwo msg) } -> ( { b | selectTwo : Maybe (SelectTwo msg) }, Cmd msg )
update msg model =
    case msg of
        SelectTwoTrigger p dd ->
            new p dd model ! []

        SelectTwoHovered hovered ->
            map (\s -> { s | hovered = hovered }) model ! []

        SelectTwoSelected msg ->
            { model | selectTwo = Nothing } ! [ msg |> Maybe.map send |> Maybe.withDefault Cmd.none ]

        SetSelectTwoSearch filter ->
            setFilter filter model ! []

        STMsg msg ->
            model ! [ send msg ]


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


location : (SelectTwoMsg msg -> msg) -> msg -> List (SelectTwoOption msg) -> List String -> Bool -> JD.Decoder msg
location sender default list parents showSearch =
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
        |> JD.map (buildDropdown sender default list showSearch)
        |> JD.map ((SelectTwoTrigger parents) >> sender)
        |> (JD.field "target" << closest "select2")


buildDropdown : (SelectTwoMsg msg -> msg) -> msg -> List (SelectTwoOption msg) -> Bool -> ( ( Float, Float ), Float ) -> SelectTwoDropdown msg
buildDropdown sender default list showSearch ( ( x, y ), width ) =
    ( sender, default, list, showSearch, x, y, width )


basicSelectOptions : (a -> msg) -> List ( a, String ) -> List (SelectTwoOption msg)
basicSelectOptions msg =
    List.map (selectOption msg)


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
