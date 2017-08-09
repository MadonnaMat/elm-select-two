module SelectTwo.Html exposing (select2, select2Dropdown, select2Css, select2Close, preventScrolling, widthGuess)

{-| this file is for all things related to select2 in the view. this will build your html and has a few helpers for some other areas


# Essentials

@docs select2Css, select2, select2Dropdown, select2Close


# Helpers

@docs preventScrolling, widthGuess

-}

import Html exposing (Html, Attribute, div, span, text, input, ul, li, strong, node, b)
import Html.Attributes exposing (class, style, id, classList, attribute, rel, href)
import Html.Events exposing (onClick, onInput)
import List.Extra
import SelectTwo
import SelectTwo.Types
    exposing
        ( SelectTwoMsg
            ( SetSelectTwoSearch
            , STNull
            , ResultScroll
            , SelectTwoHovered
            , SelectTwoSelected
            )
        , SelectTwoOption
        , GroupSelectTwoOption
        , Model
        , SelectTwoDropdown
        , SelectTwoAjaxStuff
        , ScrollInfo
        , SelectTwoConfig
        )
import SelectTwo.Private exposing (filterGroup, filterList, location, ajaxLocation, (=>), px)
import Json.Decode as JD
import Tuple3


{-| This is a stylesheet link tag to the select2 minified css, use this while developing, but it is more recommended that you use it in your head once
you compile the code instead
-}
select2Css : Html msg
select2Css =
    node "link" [ rel "stylesheet", href "//cdnjs.cloudflare.com/ajax/libs/select2/4.0.3/css/select2.min.css" ] []


{-| This is an event you put on the body of your elm view, so when a user click away it closes the dropdown
-}
select2Close : (SelectTwoMsg msg -> msg) -> Attribute msg
select2Close sender =
    onClick (sender (SelectTwoSelected Nothing))


{-| This the select2 dropdown itself, you pass it a sender and a config and you get back a select2 dropdown. Example:

    let
        testList msg =
            [ ( Just "a", "a" )
            , ( Just "b", "b" )
            , ( Just "c", "c" )
            ]
                |> SelectTwo.basicSelectOptions msg
    in
        select2 SelectTwo
            { defaults =
                (testList Test)
                    |> List.concatMap (\( _, l ) -> l)
                    |> List.filter (\l -> (Just (Test model.test)) == (l |> Tuple3.first))
            , id_ = "test-1"
            , list = testList Test
            , parents = [ "parent" ]
            , clearMsg = Just (\_ -> Test Nothing)
            , width = "300px"
            , placeholder = "Select Test"
            , disabled = False
            , showSearch = True
            , multiSelect = False
            , url = Nothing
            , data = (\_ -> "")
            , processResults = (\( _, params ) -> ( [], params ))
            , delay = 0
            }

-}
select2 : (SelectTwoMsg msg -> msg) -> SelectTwoConfig msg -> Html msg
select2 sender { defaults, list, parents, clearMsg, showSearch, width, placeholder, id_, disabled, multiSelect, url, processResults, data, delay } =
    span
        [ classList
            [ "select2 elm-select2 select2-container select2-container--default select2-container--below select2-container--focus" => True
            , "select2-container--disabled" => disabled
            ]
        , id id_
        , style [ "width" => width ]
        , if not disabled then
            case url of
                Just u ->
                    Html.Events.onWithOptions "click" preventAndStop <| (ajaxLocation id_ delay sender defaults parents (showSearch && not multiSelect) u data processResults)

                Nothing ->
                    Html.Events.onWithOptions "click" preventAndStop <| (location id_ delay sender defaults list parents (showSearch && not multiSelect))
          else
            attribute "data-blank" ""
        ]
        [ span [ class "selection" ]
            [ if multiSelect then
                multiSelectSpan sender id_ defaults list clearMsg disabled placeholder url
              else
                singleSelectSpan (defaults |> List.head) clearMsg placeholder
            ]
        ]


singleSelectSpan : Maybe (SelectTwoOption msg) -> Maybe (Maybe msg -> msg) -> String -> Html msg
singleSelectSpan default clearMsg placeholder =
    let
        ( defaultMsg, defaultText, _ ) =
            default |> Maybe.withDefault ( Nothing, text "", "" )
    in
        span [ class "select2-selection select2-selection--single" ]
            [ span [ class "select2-selection__rendered" ]
                [ if defaultText == text "" then
                    span [ class "select2-selection__placeholder" ] [ text placeholder ]
                  else
                    case clearMsg of
                        Just msg ->
                            span [ class "select2-selection__clear", onClick (msg defaultMsg) ] [ text "×" ]

                        Nothing ->
                            text ""
                , defaultText
                ]
            , span [ class "select2-selection__arrow" ] [ b [] [] ]
            ]


multiSelectSpan : (SelectTwoMsg msg -> msg) -> String -> List (SelectTwoOption msg) -> List (GroupSelectTwoOption msg) -> Maybe (Maybe msg -> msg) -> Bool -> String -> Maybe String -> Html msg
multiSelectSpan sender id_ defaults list clearMsg disabled placeholder url =
    let
        theList =
            case url of
                Just u ->
                    [ ( "", defaults ) ]

                _ ->
                    list
    in
        span [ class "select2-selection select2-selection--multiple" ]
            [ ul [ class "select2-selection__rendered" ]
                ((theList
                    |> List.concatMap Tuple.second
                    |> List.filter (flip List.member defaults)
                    |> List.map
                        (\( msg, txt, _ ) ->
                            li [ class "select2-selection__choice" ]
                                [ case clearMsg of
                                    Just clrMsg ->
                                        span [ class "select2-selection__choice__remove", onClick (clrMsg msg) ] [ text "×" ]

                                    Nothing ->
                                        text ""
                                , txt
                                ]
                        )
                 )
                    ++ [ li [ class "select2-search select2-search--inline" ]
                            [ if not disabled then
                                input
                                    [ class "select2-search__field"
                                    , onInput (SetSelectTwoSearch >> sender)
                                    , id (id_ ++ "--search")
                                    , Html.Attributes.placeholder
                                        (if (defaults |> List.length) > 0 then
                                            ""
                                         else
                                            placeholder
                                        )
                                    ]
                                    []
                              else
                                text ""
                            ]
                       ]
                )
            ]


{-| The dropdown to be shown on the page, this needs to be placed somewhere on the bottome of the view
-}
select2Dropdown : Model b msg -> Html msg
select2Dropdown model =
    case model.selectTwo of
        Just { dropdown, hovered, search, list, ajaxStuff } ->
            select2DropdownDraw dropdown hovered search list ajaxStuff

        Nothing ->
            text ""


select2DropdownDraw : SelectTwoDropdown msg -> Maybe msg -> Maybe String -> Maybe (List (GroupSelectTwoOption msg)) -> Maybe (SelectTwoAjaxStuff msg) -> Html msg
select2DropdownDraw { id_, sender, defaults, list, showSearch, x, y, width } hovered search ajaxList ajaxStuff =
    let
        theList =
            case ajaxList of
                Just l ->
                    l

                Nothing ->
                    list
    in
        span
            [ class "elm-select2 select2-container select2-container--default select2-container--open"
            , style
                [ "position" => "absolute"
                , "left" => px x
                , "top" => px y
                , "width" => px width
                ]
            ]
            [ span [ class "select2-dropdown select2-dropdown--below" ]
                [ if showSearch == True then
                    span [ class "select2-search select2-search--dropdown" ]
                        [ input
                            [ class "select2-search__field"
                            , id (id_ ++ "--search")
                            , onInput (SetSelectTwoSearch >> sender)
                            , Html.Events.onWithOptions "click" preventAndStop (JD.succeed (sender STNull))
                            ]
                            []
                        ]
                  else
                    text ""
                , span [ class "select2-results" ]
                    [ ul [ class "select2-results__options", Html.Events.on "scroll" <| scrollPosition (ResultScroll >> sender) ]
                        (listOrGroup sender defaults theList hovered search)
                    ]
                ]
            ]


scrollPosition : (ScrollInfo -> a) -> JD.Decoder a
scrollPosition wrapper =
    JD.map2 ScrollInfo
        (JD.at [ "target", "scrollHeight" ] JD.int)
        (JD.at [ "target", "scrollTop" ] JD.int)
        |> JD.map wrapper


listOrGroup : (SelectTwoMsg msg -> msg) -> List (SelectTwoOption msg) -> List (GroupSelectTwoOption msg) -> Maybe msg -> Maybe String -> List (Html msg)
listOrGroup sender defaults list hovered search =
    if List.length list == 1 && (list |> List.head |> Maybe.map (Tuple.first) |> Maybe.withDefault "") == "" then
        list
            |> List.head
            |> Maybe.map (Tuple.second)
            |> Maybe.map (List.filter (filterList search))
            |> Maybe.map (List.map (select2ListItem sender defaults hovered))
            |> Maybe.withDefault []
    else
        list
            |> List.map (filterGroup search)
            |> List.map (select2ListGroup sender defaults hovered)


select2ListGroup : (SelectTwoMsg msg -> msg) -> List (SelectTwoOption msg) -> Maybe msg -> GroupSelectTwoOption msg -> Html msg
select2ListGroup sender defaults hovered ( label, list ) =
    li [ class "select2-results__option" ]
        [ strong [ class "select2-results__group" ] [ text label ]
        , ul [ class "select2-results__options select2-results__options--nested" ]
            (list
                |> List.map (select2ListItem sender defaults hovered)
            )
        ]


select2ListItem : (SelectTwoMsg msg -> msg) -> List (SelectTwoOption msg) -> Maybe msg -> SelectTwoOption msg -> Html msg
select2ListItem sender defaults hovered option =
    let
        ( msg, display, _ ) =
            option
    in
        li
            [ classList
                [ "select2-results__option" => True
                , "select2-results__option--highlighted" => msg == hovered
                ]
            , attribute "aria-selected" (toString (defaults |> List.member option) |> String.toLower)
            , Html.Events.onMouseOver (sender (SelectTwoHovered msg))
            , onClick (sender (SelectTwoSelected msg))
            ]
            [ display ]


preventAndStop : { preventDefault : Bool, stopPropagation : Bool }
preventAndStop =
    { preventDefault = True
    , stopPropagation = True
    }


{-| Select2 only works when a parent div is not scrollable, this makes parent divs not scrollable while the dropdown is open
-}
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


{-| Use this helper method in the select2 config in order to get select2's width resolve functionality
-}
widthGuess : Float -> List (GroupSelectTwoOption msg) -> String
widthGuess font list =
    list
        |> List.concatMap Tuple.second
        |> List.map Tuple3.third
        |> List.map (String.length)
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
        |> (*) (font / 1.5)
        |> (+) 30
        |> px
