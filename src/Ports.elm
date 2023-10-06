port module Ports exposing (..)

-- PORTS


port updateStatus : (String -> msg) -> Sub msg


port sendMessage : String -> Cmd msg


port receiveMessage : (String -> msg) -> Sub msg
