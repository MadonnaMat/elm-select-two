module SelectTwo.Html exposing
    ( select2Css, select2, select2Dropdown, select2Close
    , preventScrolling, widthGuess
    )

{-| this file is for all things related to select2 in the view. this will build your html and has a few helpers for some other areas


# Essentials

@docs select2Css, select2, select2Dropdown, select2Close


# Helpers

@docs preventScrolling, widthGuess

-}

import Html exposing (Attribute, Html, b, div, input, li, node, span, strong, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id, rel, style)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import List.Extra
import SelectTwo
import SelectTwo.Private exposing (boolToString, filterGroup, filterList, flip, location, px, tuple3Second, uniqueBy)
import SelectTwo.Types
    exposing
        ( AjaxParams
        , GroupSelectTwoOption
        , Model
        , ScrollInfo
        , SelectTwoConfig
        , SelectTwoDropdown
        , SelectTwoMsg(..)
        , SelectTwoOption
        )


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
        { defaults = SelectTwo.defaultsFromList [ Test model.test ] <| testList Test
        , id_ = "test-1"
        , list = testList Test
        , clearMsg = Just (\_ -> Test Nothing)
        , width = "300px"
        , placeholder = "Select Test"
        , disabled = False
        , showSearch = True
        , multiSelect = False
        , ajax = False
        , delay = 0
        }

-}
select2 : (SelectTwoMsg msg -> msg) -> SelectTwoConfig msg -> Html msg
select2 sender { defaults, list, clearMsg, showSearch, width, placeholder, id_, disabled, multiSelect, noResultsMessage, ajax, delay, closeOnClear } =
    span
        [ classList
            [ ( "select2 elm-select2 select2-container select2-container--default select2-container--below select2-container--focus", True )
            , ( "select2-container--disabled", disabled )
            ]
        , id id_
        , style "width" width
        , if not disabled then
            Html.Events.custom "click" <| location id_ sender defaults list (showSearch && not multiSelect) noResultsMessage ajax delay

          else
            attribute "data-blank" ""
        ]
        [ span [ class "selection" ]
            [ if multiSelect then
                multiSelectSpan sender id_ defaults list clearMsg closeOnClear disabled placeholder ajax

              else
                singleSelectSpan sender (defaults |> List.head) clearMsg closeOnClear placeholder
            ]
        ]


singleSelectSpan : (SelectTwoMsg msg -> msg) -> Maybe (SelectTwoOption msg) -> Maybe (Maybe msg -> msg) -> Bool -> String -> Html msg
singleSelectSpan sender default clearMsg closeOnClear placeholder =
    let
        ( defaultMsg, defaultText, _ ) =
            default |> Maybe.withDefault ( Nothing, "", False )
    in
    span [ class "select2-selection select2-selection--single" ]
        [ span [ class "select2-selection__rendered" ]
            [ if defaultText == "" then
                span [ class "select2-selection__placeholder" ] [ text placeholder ]

              else
                case clearMsg of
                    Just msg ->
                        if closeOnClear then
                            span
                                [ class "select2-selection__clear"
                                , Html.Events.custom "click" <| preventAndStop (sender <| SelectTwoSelected <| Just (msg defaultMsg))
                                ]
                                [ text "×" ]

                        else
                            span [ class "select2-selection__clear", onClick (msg defaultMsg) ] [ text "×" ]

                    Nothing ->
                        text ""
            , text defaultText
            ]
        , span [ class "select2-selection__arrow" ] [ b [] [] ]
        ]


multiSelectSpan : (SelectTwoMsg msg -> msg) -> String -> List (SelectTwoOption msg) -> List (GroupSelectTwoOption msg) -> Maybe (Maybe msg -> msg) -> Bool -> Bool -> String -> Bool -> Html msg
multiSelectSpan sender id_ defaults list clearMsg closeOnClear disabled placeholder ajax =
    let
        theList =
            if ajax then
                [ ( "", defaults ) ]

            else
                list
    in
    span [ class "select2-selection select2-selection--multiple" ]
        [ ul [ class "select2-selection__rendered" ]
            ((theList
                |> List.concatMap Tuple.second
                |> List.filter (\a -> List.member a defaults)
                |> List.map
                    (\( msg, txt, sel ) ->
                        li [ class "select2-selection__choice" ]
                            [ case clearMsg of
                                Just clrMsg ->
                                    if closeOnClear then
                                        span
                                            [ class "select2-selection__choice__remove"
                                            , Html.Events.custom "click" <| preventAndStop (sender <| SelectTwoSelected <| Just (clrMsg msg))
                                            ]
                                            [ text "×" ]

                                    else
                                        span [ class "select2-selection__choice__remove", onClick (clrMsg msg) ] [ text "×" ]

                                Nothing ->
                                    text ""
                            , text txt
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
The second option can be a custom html builder
-}
select2Dropdown : (SelectTwoMsg msg -> msg) -> Maybe (SelectTwoOption msg -> Maybe (Html msg)) -> Model b msg -> Html msg
select2Dropdown sender customHtml model =
    case model.selectTwo of
        Just { dropdown, hovered, search, list, ajaxParams } ->
            select2DropdownDraw sender dropdown hovered search list ajaxParams customHtml

        Nothing ->
            text ""


select2DropdownDraw : (SelectTwoMsg msg -> msg) -> SelectTwoDropdown msg -> Maybe msg -> Maybe String -> List (GroupSelectTwoOption msg) -> Maybe AjaxParams -> Maybe (SelectTwoOption msg -> Maybe (Html msg)) -> Html msg
select2DropdownDraw sender { id_, defaults, list, showSearch, x, y, width, ajax, noResultsMessage } hovered search ajaxList ajaxParams customHtml =
    let
        theList =
            if ajax then
                if (Maybe.map (\{ loading } -> loading) ajaxParams |> Maybe.withDefault False) && List.isEmpty ajaxList then
                    [ ( "", [ ( Nothing, "Searching", False ) ] ) ]

                else
                    ajaxList

            else
                list
    in
    span
        [ class "elm-select2 select2-container select2-container--default select2-container--open"
        , style "position" "absolute"
        , style "left" (px x)
        , style "top" (px y)
        , style "width" (px width)
        ]
        [ span [ class "select2-dropdown select2-dropdown--below" ]
            [ if showSearch == True then
                span [ class "select2-search select2-search--dropdown" ]
                    [ input
                        [ class "select2-search__field"
                        , id (id_ ++ "--search")
                        , onInput (SetSelectTwoSearch >> sender)
                        , Html.Events.custom "click" <| preventAndStop (sender STNull)
                        ]
                        []
                    ]

              else
                text ""
            , span [ class "select2-results" ]
                [ ul [ class "select2-results__options", Html.Events.on "scroll" <| scrollPosition (ResultScroll >> sender) ]
                    (if ajax && (theList == [ ( "", [] ) ] || List.isEmpty theList) then
                        [ noResultsFound noResultsMessage ]

                     else
                        listOrGroup sender defaults theList hovered search customHtml
                    )
                ]
            ]
        ]


scrollPosition : (ScrollInfo -> a) -> JD.Decoder a
scrollPosition wrapper =
    JD.map2 ScrollInfo
        (JD.at [ "target", "scrollHeight" ] JD.int)
        (JD.map2 (+)
            (JD.at [ "target", "scrollTop" ] JD.int)
            (JD.at [ "target", "clientHeight" ] JD.int)
        )
        |> JD.map wrapper


listOrGroup : (SelectTwoMsg msg -> msg) -> List (SelectTwoOption msg) -> List (GroupSelectTwoOption msg) -> Maybe msg -> Maybe String -> Maybe (SelectTwoOption msg -> Maybe (Html msg)) -> List (Html msg)
listOrGroup sender defaults list hovered search customHtml =
    if List.length list == 1 && (list |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault "") == "" then
        list
            |> List.head
            |> Maybe.map Tuple.second
            |> Maybe.map (List.filter (filterList search))
            |> Maybe.map (uniqueBy identity)
            |> Maybe.map (List.map (select2ListItem sender defaults hovered customHtml))
            |> Maybe.withDefault []

    else
        list
            |> List.map (filterGroup search)
            |> List.map (select2ListGroup sender defaults hovered customHtml)


select2ListGroup : (SelectTwoMsg msg -> msg) -> List (SelectTwoOption msg) -> Maybe msg -> Maybe (SelectTwoOption msg -> Maybe (Html msg)) -> GroupSelectTwoOption msg -> Html msg
select2ListGroup sender defaults hovered customHtml ( label, list ) =
    li [ class "select2-results__option" ]
        [ strong [ class "select2-results__group" ] [ text label ]
        , ul [ class "select2-results__options select2-results__options--nested" ]
            (list
                |> uniqueBy identity
                |> List.map (select2ListItem sender defaults hovered customHtml)
            )
        ]


select2ListItem : (SelectTwoMsg msg -> msg) -> List (SelectTwoOption msg) -> Maybe msg -> Maybe (SelectTwoOption msg -> Maybe (Html msg)) -> SelectTwoOption msg -> Html msg
select2ListItem sender defaults hovered customHtml option =
    let
        ( msg, display, enabled ) =
            option
    in
    li
        (List.concatMap identity
            [ [ classList
                    [ ( "select2-results__option", True )
                    , ( "select2-results__option--highlighted", msg == hovered )
                    ]
              , Html.Events.onMouseOver (sender (SelectTwoHovered msg))
              ]
            , if enabled then
                [ onClick (sender (SelectTwoSelected msg))
                , attribute "aria-selected" (boolToString (defaults |> List.member option) |> String.toLower)
                ]

              else
                [ attribute "aria-disabled" (boolToString (not enabled) |> String.toLower)
                ]
            ]
        )
        [ runCustomHtml customHtml option ]


noResultsFound : Maybe String -> Html msg
noResultsFound noResults =
    li
        [ classList
            [ ( "select2-results__option", True )
            , ( "select2-results__message", True )
            ]
        ]
        [ text <| Maybe.withDefault "No Results Found" noResults ]


preventAndStop : msg -> JD.Decoder { message : msg, preventDefault : Bool, stopPropagation : Bool }
preventAndStop msg =
    JD.succeed
        { message = msg
        , preventDefault = True
        , stopPropagation = True
        }


{-| Select2 only works when a parent div is not scrollable, this makes divs not scrollable while the dropdown is open
first argument is a list of select2 ids you don't want to scroll by
-}
preventScrolling : List String -> Model b msg -> List ( String, String )
preventScrolling ids model =
    let
        prevent =
            model.selectTwo
                |> Maybe.map .id_
                |> Maybe.map (flip List.member ids)
                |> Maybe.withDefault False
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
        |> List.map tuple3Second
        |> List.map String.length
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
        |> (*) (font / 1.5)
        |> (+) 30
        |> px


runCustomHtml : Maybe (SelectTwoOption msg -> Maybe (Html msg)) -> SelectTwoOption msg -> Html msg
runCustomHtml customHtml option =
    customHtml |> Maybe.andThen (\ch -> ch option) |> Maybe.withDefault (text (option |> tuple3Second))
