module SelectTwo.Html exposing (select2, select2Dropdown, select2Css, select2Close)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import SelectTwo
import SelectTwoTypes exposing (..)
import Helpers exposing ((=>), px)


select2Css : Html msg
select2Css =
    node "link" [ rel "stylesheet", href "//cdnjs.cloudflare.com/ajax/libs/select2/4.0.3/css/select2.min.css" ] []


select2Close : (SelectTwoMsg msg -> msg) -> Attribute msg
select2Close sender =
    onClick (sender (SelectTwoSelected Nothing))


select2 : (SelectTwoMsg msg -> msg) -> SelectTwoConfig msg -> Html msg
select2 sender { default, list, parents, clearMsg, showSearch, width, placeholder } =
    let
        ( _, defaultText ) =
            list |> List.Extra.find (\( msg, _ ) -> msg == Just default) |> Maybe.withDefault ( Nothing, "" )
    in
        span
            [ class "select2 select2-container select2-container--default select2-container--below select2-container--focus"
            , style [ "width" => width ]
            , Html.Events.onWithOptions "click" preventAndStop <| (SelectTwo.location sender default list parents showSearch)
            ]
            [ span [ class "selection" ]
                [ span [ class "select2-selection select2-selection--single" ]
                    [ span [ class "select2-selection__rendered" ]
                        [ if defaultText == "" then
                            span [ class "select2-selection__placeholder" ] [ text placeholder ]
                          else
                            case clearMsg of
                                Just msg ->
                                    span [ class "select2-selection__clear", onClick (sender msg) ] [ text "×" ]

                                Nothing ->
                                    text ""
                        , text defaultText
                        ]
                    , span [ class "select2-selection__arrow" ] [ b [] [] ]
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
select2DropdownDraw ( sender, default, list, showSearch, x, y, width ) hovered search =
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
                    [ input [ class "select2-search__field", onInput (SetSelectTwoSearch >> sender) ] []
                    ]
              else
                text ""
            , span [ class "select2-results" ]
                [ ul [ class "select2-results__options" ]
                    (list
                        |> List.filter (SelectTwo.filterList search)
                        |> List.map (select2ListItem sender default hovered)
                    )
                ]
            ]
        ]


select2ListItem : (SelectTwoMsg msg -> msg) -> msg -> Maybe msg -> SelectTwoOption msg -> Html msg
select2ListItem sender default hovered ( msg, display ) =
    li
        [ classList
            [ "select2-results__option" => True
            , "select2-results__option--highlighted" => msg == hovered
            ]
        , attribute "aria-selected" (toString (Just default == msg) |> String.toLower)
        , Html.Events.onMouseOver (sender (SelectTwoHovered msg))
        , onClick (sender (SelectTwoSelected msg))
        ]
        [ text display ]


preventAndStop : { preventDefault : Bool, stopPropagation : Bool }
preventAndStop =
    { preventDefault = True
    , stopPropagation = True
    }
