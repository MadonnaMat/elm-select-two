module SelectTwoTypes exposing (..)

import Dom
import Http
import Html exposing (Html)


type SelectTwoMsg a
    = SelectTwoTrigger (List String) (SelectTwoDropdown a)
    | SelectTwoHovered (Maybe a)
    | SelectTwoSelected (Maybe a)
    | SetSelectTwoSearch String
    | SelectTwoAjax (SelectTwoAjaxStuff a) (Result Http.Error String)
    | STRes (Result Dom.Error ())
    | STMsg a
    | STNull
    | ResultScroll ScrollInfo


type alias SelectTwoConfig a =
    { defaults : List (SelectTwoOption a)
    , id_ : String
    , list : List (GroupSelectTwoOption a)
    , parents : List String
    , url : Maybe String
    , data : ( String, AjaxParams ) -> String
    , processResults : ( String, AjaxParams ) -> ( List (GroupSelectTwoOption a), AjaxParams )
    , clearMsg : Maybe (Maybe a -> a)
    , showSearch : Bool
    , width : String
    , placeholder : String
    , disabled : Bool
    , multiSelect : Bool
    }


type alias SelectTwoAjaxConfig a =
    { default : SelectTwoOption a
    , id_ : String
    , parents : List String
    , data : ( String, AjaxParams ) -> String
    , processResults : ( String, AjaxParams ) -> ( List (GroupSelectTwoOption a), AjaxParams )
    , clearMsg : Maybe a
    , showSearch : Bool
    , width : String
    , placeholder : String
    , disabled : Bool
    }


type alias AjaxParams =
    { page : Int
    , term : String
    , more : Bool
    , loading : Bool
    }


type alias GroupSelectTwoOption a =
    ( String, List (SelectTwoOption a) )


type alias SelectTwoOption a =
    ( Maybe a, Html a, String )


type alias SelectTwo a =
    { dropdown : SelectTwoDropdown a
    , hovered : Maybe a
    , search : Maybe String
    , parents : List String
    , list : Maybe (List (GroupSelectTwoOption a))
    , ajaxStuff : Maybe (SelectTwoAjaxStuff a)
    }


type alias SelectTwoDropdown a =
    ( String, SelectTwoMsg a -> a, List (SelectTwoOption a), List (GroupSelectTwoOption a), Bool, Float, Float, Float, Maybe (SelectTwoAjaxStuff a) )


type alias SelectTwoAjaxStuff a =
    ( String, ( String, AjaxParams ) -> String, ( String, AjaxParams ) -> ( List (GroupSelectTwoOption a), AjaxParams ), AjaxParams )


type alias ScrollInfo =
    { scrollHeight : Int
    , scrollTop : Int
    }
