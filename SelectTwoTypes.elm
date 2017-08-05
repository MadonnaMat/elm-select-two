module SelectTwoTypes exposing (..)

import Dom


type SelectTwoMsg a
    = SelectTwoTrigger (List String) (SelectTwoDropdown a)
    | SelectTwoHovered (Maybe a)
    | SelectTwoSelected (Maybe a)
    | SetSelectTwoSearch String
    | STRes (Result Dom.Error ())
    | STMsg a
    | STNull


type alias SelectTwoConfig a =
    { default : a
    , id_ : String
    , list : List ( String, List (SelectTwoOption a) )
    , parents : List String
    , clearMsg : Maybe a
    , showSearch : Bool
    , width : String
    , placeholder : String
    }


type alias SelectTwoMultipleConfig a =
    { defaults : List a
    , id_ : String
    , list : List ( String, List (SelectTwoOption a) )
    , parents : List String
    , clearMsg : a -> a
    , width : String
    , placeholder : String
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
    }


type alias SelectTwoDropdown a =
    ( String, SelectTwoMsg a -> a, List a, List (GroupSelectTwoOption a), Bool, Float, Float, Float )
