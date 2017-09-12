module SelectTwo.Types exposing (..)

{-| SelectTwo Types


# Types

@docs SelectTwoConfig, SelectTwoMsg, Model,SelectTwo, SelectTwoDropdown, GroupSelectTwoOption, SelectTwoOption, SelectTwoAjaxStuff, AjaxParams, ScrollInfo

-}

import Dom
import Http
import Html exposing (Html)


{-| Command Messages for SelectTwo
-}
type SelectTwoMsg msg
    = SelectTwoTrigger (List String) (SelectTwoDropdown msg)
    | SelectTwoHovered (Maybe msg)
    | SelectTwoSelected (Maybe msg)
    | SetSelectTwoSearch String
    | DelayedSelectTwoAjax String
    | SelectTwoAjax (SelectTwoAjaxStuff msg) Bool (Result Http.Error String)
    | STRes (Result Dom.Error ())
    | STMsg msg
    | STNull
    | ResultScroll ScrollInfo


{-| Model structure needed for selectTwo, all records using selectTwo should have this structure
-}
type alias Model model msg =
    { model | selectTwo : Maybe (SelectTwo msg) }


{-| Config for SelectTwo used when initializing your select2 box
-}
type alias SelectTwoConfig msg =
    { defaults : List (SelectTwoOption msg)
    , id_ : String
    , list : List (GroupSelectTwoOption msg)
    , parents : List String
    , url : Maybe String
    , data : ( String, AjaxParams ) -> String
    , processResults : ( String, AjaxParams ) -> ( List (GroupSelectTwoOption msg), AjaxParams )
    , clearMsg : Maybe (Maybe msg -> msg)
    , showSearch : Bool
    , width : String
    , placeholder : String
    , disabled : Bool
    , multiSelect : Bool
    , delay : Float
    , noResultsMessage : Maybe String
    }


{-| Parameters used in ajax calls
-}
type alias AjaxParams =
    { page : Int
    , term : String
    , more : Bool
    , loading : Bool
    }


{-| Grouped SelectTwoOption, first option in the tuple is the name of the group
-}
type alias GroupSelectTwoOption msg =
    ( String, List (SelectTwoOption msg) )


{-| Rows in a select table, first option is the command message to be sent, second is the html to be displayed, and third is the string to search on, and the fourth is if it is disabled or not
-}
type alias SelectTwoOption msg =
    ( Maybe msg, Html msg, String, Bool )


{-| Structure created in users model when select2 is activated
-}
type alias SelectTwo msg =
    { dropdown : SelectTwoDropdown msg
    , hovered : Maybe msg
    , search : Maybe String
    , parents : List String
    , list : List (GroupSelectTwoOption msg)
    , ajaxStuff : Maybe (SelectTwoAjaxStuff msg)
    }


{-| Data to generate the dropdown
-}
type alias SelectTwoDropdown msg =
    { id_ : String
    , sender : SelectTwoMsg msg -> msg
    , defaults : List (SelectTwoOption msg)
    , list : List (GroupSelectTwoOption msg)
    , showSearch : Bool
    , x : Float
    , y : Float
    , width : Float
    , ajaxStuff : Maybe (SelectTwoAjaxStuff msg)
    , delay : Float
    , isAjax : Bool
    , noResultsMessage : Maybe String
    }


{-| Things needed to make Ajax calls
-}
type alias SelectTwoAjaxStuff msg =
    ( String, ( String, AjaxParams ) -> String, ( String, AjaxParams ) -> ( List (GroupSelectTwoOption msg), AjaxParams ), AjaxParams )


{-| -}
type alias ScrollInfo =
    { scrollHeight : Int
    , scrollTop : Int
    }
