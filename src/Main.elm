module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Ports exposing (..)
import Status exposing (Status)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { draft : String
    , messages : List String
    , status : Status
    }


initialModel : Model
initialModel =
    Model "" [] Status.Connecting


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = DraftChanged String
    | Send
    | Recv String
    | ReceiveStatus Status



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DraftChanged draft ->
            ( { model | draft = draft }
            , Cmd.none
            )

        ReceiveStatus status ->
            ( { model | status = status }, Cmd.none )

        Send ->
            ( { model | draft = "" }
            , sendMessage model.draft
            )

        Recv message ->
            ( { model | messages = model.messages ++ [ message ] }
            , Cmd.none
            )



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions { status } =
    Sub.batch
        [ if Status.isSuccess status then
            receiveMessage Recv

          else
            Sub.none
        , updateStatus (Status.fromString >> ReceiveStatus)
        ]



-- VIEW


view : Model -> Html Msg
view { messages, draft, status } =
    div []
        [ h1 [] [ text "Echo Chat" ]
        , div []
            [ strong [] [ text "Status: " ]
            , text <| Status.toString status
            ]
        , ul []
            (List.map (\msg -> li [] [ text msg ]) messages)
        , input
            [ type_ "text"
            , placeholder "Draft"
            , onInput DraftChanged
            , on "keydown" (ifIsEnter Send)
            , value draft
            , disabled <| status /= Status.Success
            ]
            []
        , button [ disabled <| status /= Status.Success, onClick Send ] [ text "Send" ]
        ]



-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg

                else
                    D.fail "some other key"
            )
