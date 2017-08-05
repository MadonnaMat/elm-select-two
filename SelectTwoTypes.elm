module SelectTwoTypes exposing (..)

import Dom
import Http


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
    { default : a
    , id_ : String
    , list : List (GroupSelectTwoOption a)
    , parents : List String
    , clearMsg : Maybe a
    , showSearch : Bool
    , width : String
    , placeholder : String
    }


type alias SelectTwoAjaxConfig a =
    { default : SelectTwoOption a
    , id_ : String
    , parents : List String
    , url : String
    , data : ( String, AjaxParams ) -> String
    , processResults : ( String, AjaxParams ) -> ( List (GroupSelectTwoOption a), AjaxParams )
    , clearMsg : Maybe a
    , showSearch : Bool
    , width : String
    , placeholder : String
    }


type alias SelectTwoMultipleConfig a =
    { defaults : List a
    , id_ : String
    , list : List (GroupSelectTwoOption a)
    , parents : List String
    , clearMsg : a -> a
    , width : String
    , placeholder : String
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
    ( Maybe a, String )


type alias SelectTwo a =
    { dropdown : SelectTwoDropdown a
    , hovered : Maybe a
    , search : Maybe String
    , parents : List String
    , list : Maybe (List (GroupSelectTwoOption a))
    , ajaxStuff : Maybe (SelectTwoAjaxStuff a)
    }


type alias SelectTwoDropdown a =
    ( String, SelectTwoMsg a -> a, List a, List (GroupSelectTwoOption a), Bool, Float, Float, Float, Maybe (SelectTwoAjaxStuff a) )


type alias SelectTwoAjaxStuff a =
    ( String, ( String, AjaxParams ) -> String, ( String, AjaxParams ) -> ( List (GroupSelectTwoOption a), AjaxParams ), AjaxParams )


type alias ScrollInfo =
    { scrollHeight : Int
    , scrollTop : Int
    }
