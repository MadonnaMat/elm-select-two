module SelectTwo.Html exposing (select2, select2Multiple, select2Dropdown, select2Css, select2Close)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import SelectTwo
import SelectTwoTypes exposing (..)
import Helpers exposing ((=>), px)
import Json.Decode as JD


select2Css : Html msg
select2Css =
    node "link" [ rel "stylesheet", href "//cdnjs.cloudflare.com/ajax/libs/select2/4.0.3/css/select2.min.css" ] []


select2Close : (SelectTwoMsg msg -> msg) -> Attribute msg
select2Close sender =
    onClick (sender (SelectTwoSelected Nothing))


select2 : (SelectTwoMsg msg -> msg) -> SelectTwoConfig msg -> Html msg
select2 sender { default, list, parents, clearMsg, showSearch, width, placeholder, id_ } =
    let
        ( _, defaultText ) =
            list |> List.concatMap (\( _, l ) -> l) |> List.Extra.find (\( msg, _ ) -> msg == Just default) |> Maybe.withDefault ( Nothing, "" )
    in
        span
            [ class "select2 select2-container select2-container--default select2-container--below select2-container--focus"
            , id id_
            , style [ "width" => width ]
            , Html.Events.onWithOptions "click" preventAndStop <| (SelectTwo.location id_ sender [ default ] list parents showSearch)
            ]
            [ span [ class "selection" ]
                [ span [ class "select2-selection select2-selection--single" ]
                    [ span [ class "select2-selection__rendered" ]
                        [ if defaultText == "" then
                            span [ class "select2-selection__placeholder" ] [ text placeholder ]
                          else
                            case clearMsg of
                                Just msg ->
                                    span [ class "select2-selection__clear", onClick (sender (STMsg msg)) ] [ text "×" ]

                                Nothing ->
                                    text ""
                        , text defaultText
                        ]
                    , span [ class "select2-selection__arrow" ] [ b [] [] ]
                    ]
                ]
            ]


select2Multiple : (SelectTwoMsg msg -> msg) -> SelectTwoMultipleConfig msg -> Html msg
select2Multiple sender { defaults, list, parents, clearMsg, width, placeholder, id_ } =
    span
        [ class "select2 select2-container select2-container--default select2-container--below select2-container--focus"
        , style [ "width" => width ]
        , id id_
        , Html.Events.onWithOptions "click" preventAndStop <| (SelectTwo.location id_ sender defaults list parents False)
        ]
        [ span [ class "selection" ]
            [ span [ class "select2-selection select2-selection--multiple" ]
                [ ul [ class "select2-selection__rendered" ]
                    ((list
                        |> List.concatMap Tuple.second
                        |> List.filter (\l -> defaults |> List.map Just |> List.member (Tuple.first l))
                        |> List.map
                            (\( msg, txt ) ->
                                case msg of
                                    Just ms ->
                                        li [ class "select2-selection__choice" ]
                                            [ span [ class "select2-selection__choice__remove", onClick (sender (STMsg (clearMsg ms))) ] [ text "×" ]
                                            , text txt
                                            ]

                                    Nothing ->
                                        text ""
                            )
                     )
                        ++ [ li [ class "select2-search select2-search--inline" ]
                                [ input
                                    [ class "select2-search__field"
                                    , onInput (SetSelectTwoSearch >> sender)
                                    , id (id_ ++ "--search")
                                    ]
                                    []
                                ]
                           ]
                    )
                ]
            ]
        ]


select2Dropdown : { b | selectTwo : Maybe (SelectTwo msg) } -> Html msg
select2Dropdown model =
    case model.selectTwo of
        Just { dropdown, hovered, search } ->
            select2DropdownDraw dropdown hovered search

        Nothing ->
            text ""


select2DropdownDraw : SelectTwoDropdown msg -> Maybe msg -> Maybe String -> Html msg
select2DropdownDraw ( id_, sender, defaults, list, showSearch, x, y, width ) hovered search =
    span
        [ class "select2-container select2-container--default select2-container--open"
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
                [ ul [ class "select2-results__options" ]
                    (listOrGroup sender defaults list hovered search)
                ]
            ]
        ]


listOrGroup : (SelectTwoMsg msg -> msg) -> List msg -> List (GroupSelectTwoOption msg) -> Maybe msg -> Maybe String -> List (Html msg)
listOrGroup sender defaults list hovered search =
    if List.length list == 1 && (list |> List.head |> Maybe.map (Tuple.first) |> Maybe.withDefault "") == "" then
        list
            |> List.head
            |> Maybe.map (Tuple.second)
            |> Maybe.map (List.filter (SelectTwo.filterList search))
            |> Maybe.map (List.map (select2ListItem sender defaults hovered))
            |> Maybe.withDefault []
    else
        list
            |> List.map (SelectTwo.filterGroup search)
            |> List.map (select2ListGroup sender defaults hovered)


select2ListGroup : (SelectTwoMsg msg -> msg) -> List msg -> Maybe msg -> GroupSelectTwoOption msg -> Html msg
select2ListGroup sender defaults hovered ( label, list ) =
    li [ class "select2-results__option" ]
        [ strong [ class "select2-results__group" ] [ text label ]
        , ul [ class "select2-results__options select2-results__options--nested" ]
            (list
                |> List.map (select2ListItem sender defaults hovered)
            )
        ]


select2ListItem : (SelectTwoMsg msg -> msg) -> List msg -> Maybe msg -> SelectTwoOption msg -> Html msg
select2ListItem sender defaults hovered ( msg, display ) =
    li
        [ classList
            [ "select2-results__option" => True
            , "select2-results__option--highlighted" => msg == hovered
            ]
        , attribute "aria-selected" (toString (defaults |> List.map Just |> List.member msg) |> String.toLower)
        , Html.Events.onMouseOver (sender (SelectTwoHovered msg))
        , onClick (sender (SelectTwoSelected msg))
        ]
        [ text display ]


preventAndStop : { preventDefault : Bool, stopPropagation : Bool }
preventAndStop =
    { preventDefault = True
    , stopPropagation = True
    }
