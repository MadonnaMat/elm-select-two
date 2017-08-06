module SelectTwoTypes exposing (..)

import Dom
import Http
import Html exposing (Html)


type SelectTwoMsg a
    = SelectTwoTrigger (List String) (SelectTwoDropdown a)
    | SelectTwoHovered (Maybe a)
    | SelectTwoSelected (Maybe a)
    | SetSelectTwoSearch String
    | DelayedSelectTwoAjax String
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
    , delay : Float
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
    { id_ : String
    , sender : SelectTwoMsg a -> a
    , defaults : List (SelectTwoOption a)
    , list : List (GroupSelectTwoOption a)
    , showSearch : Bool
    , x : Float
    , y : Float
    , width : Float
    , ajaxStuff : Maybe (SelectTwoAjaxStuff a)
    , delay : Float
    }


type alias SelectTwoAjaxStuff a =
    ( String, ( String, AjaxParams ) -> String, ( String, AjaxParams ) -> ( List (GroupSelectTwoOption a), AjaxParams ), AjaxParams )


type alias ScrollInfo =
    { scrollHeight : Int
    , scrollTop : Int
    }
