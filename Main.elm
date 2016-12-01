module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode exposing (int, string, float, list, Decoder)
import Json.Encode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Task
import Time exposing (Time, second)
import Utils


uri =
    "http://demo-kinto.scalingo.io/v1/buckets/f9aff62e-d2d5-479b-a9c7-37f4baf33d19/collections/chat/records"


querystring =
    "?_limit=10&_sort=-last_modified"


type Msg
    = NewMessage Message
    | NewAuthorName String
    | SendMessage
    | MessagePosted (Result Http.Error String)
    | PrepareMessage String
    | InitialList (Result Http.Error Data)
    | Tick Time


type alias Model =
    { author : String
    , messages : List Message
    , prepareMessage : String
    , currentTime : Time
    }


type alias Data =
    { data : List Message
    }


type alias Message =
    { author : String
    , message : String
    , last_modified : Time
    }


getData : Cmd Msg
getData =
    Http.send InitialList <|
        Http.get (uri ++ querystring) listDecoder


listDecoder : Decoder Data
listDecoder =
    decode Data
        |> required "data" (list modelDecoder)


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
                [ ( "data"
                  , Json.Encode.object
                        [ ( "author", Json.Encode.string author )
                        , ( "message", Json.Encode.string message )
                        ]
                  )
                ]
    in
        Http.send MessagePosted <|
            Http.request
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" "Basic ZHVtbXk6cmVxdWVzdA=="
                    , Http.header "Content-Type" "application/json"
                    ]
                , url = uri
                , body = Http.jsonBody jsonMessage
                , expect = Http.expectStringResponse (\{ body } -> Ok body)
                , timeout = Nothing
                , withCredentials = False
                }


handleRequestComplete : Result Http.Error (List String) -> Cmd Msg
handleRequestComplete _ =
    getData


init : ( Model, Cmd Msg )
init =
    ( Model "Guest" [] "" 0, getData )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialList results ->
            case (Debug.log "results:" results) of
                Ok new ->
                    ( { model | messages = new.data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

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

        MessagePosted (Ok deployed) ->
            ( { model | prepareMessage = "" }, getData )

        MessagePosted (Err error) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second Tick
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
