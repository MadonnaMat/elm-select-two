module Helpers exposing (..)

import Json.Decode as JD


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
infixl 0 =>


px : number -> String
px =
    toString >> flip (++) "px"


hasClass : String -> JD.Decoder a -> JD.Decoder a
hasClass class =
    decodeIf (Maybe.map (String.split " " >> List.member class) >> Maybe.withDefault False) className


className : JD.Decoder (Maybe String)
className =
    JD.field "className" <| JD.maybe JD.string


decodeIf : (b -> Bool) -> JD.Decoder b -> JD.Decoder a -> JD.Decoder a
decodeIf pred inDec outDec =
    let
        cond x =
            if pred x then
                outDec
            else
                JD.fail ""
    in
        JD.andThen cond inDec


closest : String -> JD.Decoder a -> JD.Decoder a
closest class decoder =
    let
        body () =
            JD.oneOf
                [ hasClass class decoder
                , JD.field "parentElement" <| closest class decoder
                ]
    in
        JD.lazy body
