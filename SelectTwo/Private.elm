module SelectTwo.Private exposing (..)

import SelectTwo.Types exposing (..)
import Tuple
import List.Extra
import Json.Decode as JD


filterGroup : Maybe String -> GroupSelectTwoOption a -> GroupSelectTwoOption a
filterGroup filter list =
    list |> Tuple.mapSecond (List.filter (filterList filter))


filterList : Maybe String -> SelectTwoOption a -> Bool
filterList filter ( _, _, text ) =
    filter
        |> Maybe.andThen ((String.toLower) >> (flip String.contains (text |> String.toLower)) >> Just)
        |> Maybe.withDefault True


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


px : number -> String
px =
    toString >> flip (++) "px"


hasClass : String -> JD.Decoder a -> JD.Decoder a
hasClass class =
    decodeIf (Maybe.map (String.split " " >> List.member class) >> Maybe.withDefault False) className


className : JD.Decoder (Maybe String)
className =
    JD.field "className" <| JD.maybe JD.string


decodeIf : (b -> Bool) -> JD.Decoder b -> JD.Decoder a -> JD.Decoder a
decodeIf pred inDec outDec =
    let
        cond x =
            if pred x then
                outDec
            else
                JD.fail ""
    in
        JD.andThen cond inDec


closest : String -> JD.Decoder a -> JD.Decoder a
closest class decoder =
    let
        body () =
            JD.oneOf
                [ hasClass class decoder
                , JD.field "parentElement" <| closest class decoder
                ]
    in
        JD.lazy body


asTuple : (a -> b) -> (a -> c) -> a -> ( b, c )
asTuple f1 f2 a =
    ( f1 a, f2 a )
