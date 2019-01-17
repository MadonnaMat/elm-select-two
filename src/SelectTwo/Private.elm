module SelectTwo.Private exposing (asTuple, boolToString, buildDropdown, defaultParams, filterGroup, filterList, flip, groupWhile, location, px, tuple3First, tuple3Init, tuple3Second, tuple3Third, uncurry3, uniqueBy)

import Json.Decode as JD
import List.Extra
import SelectTwo.Types exposing (..)
import Tuple


filterGroup : Maybe String -> GroupSelectTwoOption a -> GroupSelectTwoOption a
filterGroup filter list =
    list |> Tuple.mapSecond (List.filter (filterList filter))


filterList : Maybe String -> SelectTwoOption a -> Bool
filterList filter ( _, text, _ ) =
    filter
        |> Maybe.andThen (String.toLower >> (\a -> String.contains a (text |> String.toLower)) >> Just)
        |> Maybe.withDefault True


location : String -> (SelectTwoMsg msg -> msg) -> List (SelectTwoOption msg) -> List (GroupSelectTwoOption msg) -> Bool -> Maybe String -> Bool -> Float -> JD.Decoder { message : msg, preventDefault : Bool, stopPropagation : Bool }
location id_ sender defaults list showSearch noResultsMessage ajax delay =
    JD.succeed
        (buildDropdown id_
            defaults
            list
            showSearch
            ajax
            delay
            noResultsMessage
        )
        |> JD.map (SelectTwoTrigger >> sender)
        |> JD.map
            (\msg ->
                { message = msg
                , preventDefault = True
                , stopPropagation = True
                }
            )


buildDropdown : String -> List (SelectTwoOption msg) -> List (GroupSelectTwoOption msg) -> Bool -> Bool -> Float -> Maybe String -> SelectTwoDropdown msg
buildDropdown id_ defaults list showSearch ajax delay noResultsMessage =
    { id_ = id_
    , defaults = defaults
    , list = list
    , showSearch = showSearch
    , x = 0
    , y = 0
    , width = 0
    , ajax = ajax
    , delay = delay
    , noResultsMessage = noResultsMessage
    }


px : Float -> String
px =
    String.fromFloat >> (\a -> (++) a "px")


asTuple : (a -> b) -> (a -> c) -> a -> ( b, c )
asTuple f1 f2 a =
    ( f1 a, f2 a )


defaultParams : String -> AjaxParams
defaultParams filter =
    { page = 1, term = filter, more = False, loading = True }


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 f ( a, b, c ) =
    f a b c


tuple3First : ( a, b, c ) -> a
tuple3First ( a, b, c ) =
    a


tuple3Second : ( a, b, c ) -> b
tuple3Second ( a, b, c ) =
    b


tuple3Third : ( a, b, c ) -> c
tuple3Third ( a, b, c ) =
    c


tuple3Init : ( a, b, c ) -> ( a, b )
tuple3Init ( a, b, c ) =
    ( a, b )


groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile eq xs_ =
    case xs_ of
        [] ->
            []

        x :: xs ->
            let
                ( ys, zs ) =
                    List.Extra.span (eq x) xs
            in
            (x :: ys) :: groupWhile eq zs


uniqueBy : (a -> b) -> List a -> List a
uniqueBy f list =
    uniqueHelp f [] list []


uniqueHelp : (a -> b) -> List b -> List a -> List a -> List a
uniqueHelp f existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                computedFirst =
                    f first
            in
            if List.member computedFirst existing then
                uniqueHelp f existing rest accumulator

            else
                uniqueHelp f (computedFirst :: existing) rest (first :: accumulator)


boolToString : Bool -> String
boolToString a =
    case a of
        True ->
            "True"

        False ->
            "False"


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b
