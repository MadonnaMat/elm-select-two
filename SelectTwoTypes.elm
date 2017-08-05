module SelectTwoTypes exposing (..)


type SelectTwoMsg a
    = SelectTwoTrigger (List String) (SelectTwoDropdown a)
    | SelectTwoHovered (Maybe a)
    | SelectTwoSelected (Maybe a)
    | SetSelectTwoSearch String
    | STMsg a


type alias SelectTwoConfig a =
    { default : a
    , list : List (SelectTwoOption a)
    , parents : List String
    , clearMsg : Maybe (SelectTwoMsg a)
    , showSearch : Bool
    , width : String
    , placeholder : String
    }


type alias SelectTwoOption a =
    ( Maybe a, String )


type alias SelectTwo a =
    { dropdown : SelectTwoDropdown a
    , hovered : Maybe a
    , search : Maybe String
    , parents : List String
    }


type alias SelectTwoDropdown a =
    ( SelectTwoMsg a -> a, a, List (SelectTwoOption a), Bool, Float, Float, Float )
