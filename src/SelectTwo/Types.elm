module SelectTwo.Types exposing (SelectTwoConfig, SelectTwoMsg(..), Model, SelectTwo, SelectTwoDropdown, GroupSelectTwoOption, SelectTwoOption, AjaxParams, ScrollInfo)

{-| SelectTwo Types


# Types

@docs SelectTwoConfig, SelectTwoMsg, Model, SelectTwo, SelectTwoDropdown, GroupSelectTwoOption, SelectTwoOption, AjaxParams, ScrollInfo

-}

import Browser.Dom as Dom
import Html exposing (Html)
import Http


{-| Command Messages for SelectTwo
-}
type SelectTwoMsg msg
    = SelectTwoTrigger (SelectTwoDropdown msg)
    | SelectTwoOpen (SelectTwoDropdown msg) (Result Dom.Error Dom.Element)
    | SelectTwoHovered (Maybe msg)
    | SelectTwoSelected (Maybe msg)
    | SetSelectTwoSearch String
    | DelayedSelectTwoAjax String
    | STRes (Result Dom.Error ())
    | STMsg msg
    | STNull
    | SentAjax String AjaxParams Bool
    | ResultScroll ScrollInfo
    | ForceMajor


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
    , clearMsg : Maybe (Maybe msg -> msg)
    , showSearch : Bool
    , width : String
    , placeholder : String
    , disabled : Bool
    , multiSelect : Bool
    , noResultsMessage : Maybe String
    , ajax : Bool
    , delay : Float
    , closeOnClear : Bool
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


{-| Rows in a select table, first option is the command message to be sent, second is the html to be displayed and the string to search on, and the third is if it is disabled or not
-}
type alias SelectTwoOption msg =
    ( Maybe msg, String, Bool )


{-| Structure created in users model when select2 is activated
-}
type alias SelectTwo msg =
    { dropdown : SelectTwoDropdown msg
    , hovered : Maybe msg
    , search : Maybe String
    , list : List (GroupSelectTwoOption msg)
    , ajax : Bool
    , id_ : String
    , ajaxParams : Maybe AjaxParams
    }


{-| Data to generate the dropdown
-}
type alias SelectTwoDropdown msg =
    { id_ : String
    , defaults : List (SelectTwoOption msg)
    , list : List (GroupSelectTwoOption msg)
    , showSearch : Bool
    , x : Float
    , y : Float
    , width : Float
    , ajax : Bool
    , delay : Float
    , noResultsMessage : Maybe String
    }


{-| -}
type alias ScrollInfo =
    { scrollHeight : Int
    , scrollTop : Int
    }
