module Buffer exposing (..)

import Array exposing (Array)


type alias ZipArray a b =
    { head : Array a
    , zip : b
    , zipAt : Int
    , tail : Array a
    , length : Int
    }
