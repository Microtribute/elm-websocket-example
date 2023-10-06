module Status exposing (Status(..), fromString, isClosed, isError, isSuccess, isUnknown, toString)


type Status
    = Success
    | Timeout
    | Failure
    | Closed
    | Unknown
    | Connecting


isSuccess : Status -> Bool
isSuccess =
    (==) Success


isClosed : Status -> Bool
isClosed =
    (==) Closed


isError : Status -> Bool
isError status =
    List.member status [ Timeout, Failure ]


isUnknown : Status -> Bool
isUnknown =
    (==) Unknown


toString : Status -> String
toString status =
    case status of
        Success ->
            "Success"

        Failure ->
            "Failure"

        Timeout ->
            "Timeout"

        Connecting ->
            "Connecting"

        Closed ->
            "Closed"

        _ ->
            "Unknown"


fromString : String -> Status
fromString str =
    let
        s =
            String.toLower str
    in
    case s of
        "success" ->
            Success

        "failure" ->
            Failure

        "timeout" ->
            Timeout

        "connecting" ->
            Connecting

        "closed" ->
            Closed

        _ ->
            Unknown
