port module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode exposing (int, string, float, list, Decoder, decodeString)
import Json.Encode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Kinto
import Task
import Time exposing (Time, second)
import Utils


uri =
    "https://kinto.ticabri.com/v1/buckets/kintochat/collections/4815162342/records"


querystring =
    "?_limit=10&_sort=-last_modified"


client : Kinto.Client
client =
    Kinto.client
        "https://kinto.ticabri.com/v1/"
        (Kinto.Basic "random" "user")


recordResource : Kinto.Resource Message
recordResource =
    Kinto.recordResource "kintochat" "4815162342" modelDecoder


type Msg
    = NewMessage Message
    | NewAuthorName String
    | SendMessage
    | MessagePosted (Result Kinto.Error Message)
    | PrepareMessage String
    | InitialList (Result Kinto.Error (List Message))
    | Tick Time


type alias Model =
    { author : String
    , messages : List Message
    , prepareMessage : String
    , currentTime : Time
    , error : Maybe String
    }


type alias Message =
    { author : String
    , message : String
    , last_modified : Time
    }


getData : Cmd Msg
getData =
    client
        |> Kinto.getList recordResource
        |> Kinto.sortBy [ "-last_modified" ]
        |> Kinto.send InitialList


modelDecoder : Decoder Message
modelDecoder =
    decode Message
        |> required "author" string
        |> required "message" string
        |> required "last_modified" float


sendMessage : String -> String -> Cmd Msg
sendMessage author message =
    let
        jsonMessage =
            Json.Encode.object
                [ ( "author", Json.Encode.string author )
                , ( "message", Json.Encode.string message )
                ]
    in
        client
            |> Kinto.create recordResource jsonMessage
            |> Kinto.send MessagePosted


init : ( Model, Cmd Msg )
init =
    ( Model "Guest" [] "" 0 Nothing, getData )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialList (Ok messages) ->
            ( { model | error = Nothing, messages = (List.reverse messages) }, Cmd.none )

        InitialList (Err error) ->
            { model | error = Just <| toString error } ! []

        NewMessage message ->
            ( { model | messages = (model.messages ++ [ message ]) }, Cmd.none )

        NewAuthorName author ->
            ( { model | author = author }, Cmd.none )

        PrepareMessage message ->
            ( { model | prepareMessage = message }, Cmd.none )

        SendMessage ->
            ( model, (sendMessage model.author model.prepareMessage) )

        Tick time ->
            ( { model | currentTime = time }, Cmd.none )

        MessagePosted (Ok message) ->
            ( { model | error = Nothing, prepareMessage = "" }, getData )

        MessagePosted (Err error) ->
            ( { model | error = Just <| toString error }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second Tick
        , newMessage NewMessage
        ]


view : Model -> Html.Html Msg
view model =
    Html.section [ Html.Attributes.class "app-card mdl-card mdl-shadow--2dp" ]
        [ Html.div [ Html.Attributes.class "name" ]
            [ Html.form [ Html.Attributes.id "message-form", Html.Attributes.action "#" ]
                [ Html.div
                    [ Html.Attributes.class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label"
                    ]
                    [ Html.input
                        [ Html.Attributes.class "mdl-textfield__input"
                        , Html.Events.onInput NewAuthorName
                        , Html.Attributes.value model.author
                        , Html.Attributes.tabindex 1
                        ]
                        []
                    , Html.label
                        [ Html.Attributes.class "mdl-textfield__label"
                        , Html.Attributes.for "messages"
                        ]
                        [ Html.text "My name is" ]
                    ]
                ]
            ]
        , Html.div [ Html.Attributes.class "mdl-card__supporting-text" ]
            [ Html.div [ Html.Attributes.id "messages" ]
                [ model.messages
                    |> List.map
                        (\l ->
                            Html.li [ Html.Attributes.class "mdl-list__item mdl-list__item--three-line" ]
                                [ Html.span
                                    [ Html.Attributes.class "mdl-list__item-primary-content"
                                    ]
                                    [ Html.i
                                        [ Html.Attributes.class "material-icons mdl-list__item-avatar"
                                        ]
                                        [ Html.text "person" ]
                                    , Html.span [] [ Html.text l.author ]
                                    , Html.span [ Html.Attributes.class "time-ago" ] [ Html.text (Utils.timeAgo l.last_modified model.currentTime) ]
                                    , Html.span
                                        [ Html.Attributes.class "mdl-list__item-text-body"
                                        ]
                                        [ Html.text l.message ]
                                    ]
                                ]
                        )
                    |> Html.ul [ Html.Attributes.class "mdl-list" ]
                ]
            , Html.form
                [ Html.Attributes.id "message-form"
                , Html.Attributes.action "#"
                ]
                [ Html.div
                    [ Html.Attributes.class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label"
                    ]
                    [ Html.input
                        [ Html.Attributes.class "mdl-textfield__input"
                        , Html.Attributes.type_ "text"
                        , Html.Attributes.id "message"
                        , Html.Events.onInput PrepareMessage
                        , Html.Attributes.value model.prepareMessage
                        , Html.Attributes.tabindex 2
                        ]
                        []
                    , Html.label
                        [ Html.Attributes.class "mdl-textfield__label"
                        , Html.Attributes.for "messages"
                        ]
                        [ Html.text "Write someting nice…" ]
                    ]
                , Html.button
                    [ Html.Attributes.id "submit"
                    , Html.Events.onClick SendMessage
                    , Html.Attributes.class "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect"
                    , Html.Attributes.tabindex 3
                    ]
                    [ Html.text "Send" ]
                ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- ports


port newMessage : (Message -> msg) -> Sub msg
