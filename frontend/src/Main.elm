port module Main exposing (Model, Msg, StartFlag(..), toMain)

import Browser
import Codec
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Indigo
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Parser exposing ((|.), (|=), DeadEnd, Parser)
import Random
import Return
import SubModule
import Task
import Tasks exposing (Effect(..), MonthlyRepeatOn(..), Msg(..), RepeatPattern(..), Task(..), Tasks, TrackerUpdate(..))
import Time
import Urbit exposing (InMsgData(..))
import Urbit.Encoding.Atom exposing (Atom)
import Urbit.Encoding.Phonemic



-- MAIN


type StartFlag
    = Dev (Result (List DeadEnd) Atom) String String
    | Live


toMain : StartFlag -> Program Int Model Msg
toMain startFlag =
    Browser.element
        { init = init
        , view = view
        , update = update startFlag
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { timeZone : Time.Zone
    , time : Time.Posix
    , session : Maybe Urbit.Session
    , notifications : List Notification
    , tasksState : Tasks.State
    }


type Notification
    = PokeFailed String
    | UnknownDiff


init : Int -> ( Model, Cmd Msg )
init timeMillis =
    let
        time : Time.Posix
        time =
            Time.millisToPosix timeMillis
    in
    ( { timeZone = Time.utc
      , time = time
      , session = Nothing
      , notifications = []
      , tasksState = Tasks.init
      }
    , Cmd.batch
        [ Time.here
            |> Task.perform GotTimeZone
        , Urbit.genChannelId time
            |> Random.generate GotChannelId
        ]
    )



-- UPDATE


type Msg
    = Ignore
    | GotTime Time.Posix
    | GotTimeZone Time.Zone
    | GotChannelId String
    | GotShipJs String (Result Http.Error String)
    | UrbitConnected (Result Http.Error Urbit.Session)
    | GotUrbitMessage (Result Decode.Error Urbit.InMsg)
    | TasksMsg Tasks.Msg
    | TasksEffect Tasks.Effect
    | NotificationClosed Int


update : StartFlag -> Msg -> Model -> ( Model, Cmd Msg )
update startFlag msg model =
    case msg of
        Ignore ->
            ( model, Cmd.none )

        GotTime time ->
            ( { model | time = time }, Cmd.none )

        GotTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.none )

        GotChannelId channelId ->
            ( model
            , case startFlag of
                Dev (Ok ship) url code ->
                    Urbit.connect
                        { ship = ship
                        , url = url
                        , channelId = channelId
                        , code = code
                        }
                        UrbitConnected

                Dev (Err _) _ _ ->
                    Cmd.none

                Live ->
                    Http.get
                        { url = "/session.js"
                        , expect = Http.expectString (GotShipJs channelId)
                        }
            )

        GotShipJs channelId (Ok shipJsString) ->
            let
                shipResult : Result (List Parser.DeadEnd) Atom
                shipResult =
                    Parser.run shipJsStringParser shipJsString
                        |> Result.andThen ((++) "~" >> Urbit.Encoding.Phonemic.fromPatp)
            in
            case shipResult of
                Ok ship ->
                    ( model
                    , Urbit.connectUnauth
                        { ship = ship
                        , url = ""
                        , channelId = channelId
                        }
                        UrbitConnected
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotShipJs _ (Err _) ->
            ( model, Cmd.none )

        UrbitConnected (Ok session) ->
            subscribeToTasksUpdates (Urbit.ship session)
                |> Urbit.send session (\_ -> Ignore)
                |> Return.map (\newSession -> { model | session = Just newSession })
                |> Return.command (Urbit.setupEventSource setupUrbitEventSource session)

        UrbitConnected (Err _) ->
            ( model, Cmd.none )

        GotUrbitMessage (Ok urbitMsg) ->
            case model.session of
                Just session ->
                    let
                        ( newModel, outMsgs ) =
                            updateUrbit (Urbit.ship session) urbitMsg.data model
                    in
                    Urbit.ack urbitMsg.lastEventId
                        :: outMsgs
                        |> Urbit.sendBatch session (\_ -> Ignore)
                        |> Return.map
                            (\newSession ->
                                { newModel | session = Just newSession }
                            )

                Nothing ->
                    ( model, Cmd.none )

        GotUrbitMessage (Err _) ->
            ( model, Cmd.none )

        TasksMsg subMsg ->
            Tasks.update model subMsg model.tasksState
                |> SubModule.updateWithEffect
                    { toMsg = TasksMsg
                    , effectToMsg = TasksEffect
                    , toModel = \tasksState -> { model | tasksState = tasksState }
                    }

        TasksEffect subEffect ->
            case ( model.session, subEffect ) of
                ( Just session, SendUpdate trackerUpdate ) ->
                    Urbit.poke
                        { ship = Urbit.ship session
                        , app = appName
                        , mark = "tracker-update-0"
                        , json = Codec.encoder Tasks.trackerUpdateCodec trackerUpdate
                        }
                        |> Urbit.send session (\_ -> Ignore)
                        |> Return.map (\newSession -> { model | session = Just newSession })

                ( Nothing, _ ) ->
                    ( model, Cmd.none )

        NotificationClosed index ->
            ( { model | notifications = List.removeAt index model.notifications }, Cmd.none )


updateUrbit : Atom -> Urbit.InMsgData -> Model -> ( Model, List Urbit.OutMsg )
updateUrbit ship msg model =
    case msg of
        Poke _ response ->
            case response of
                Ok () ->
                    ( model, [] )

                Err error ->
                    ( { model | notifications = PokeFailed error :: model.notifications }, [] )

        Subscribe _ _ ->
            ( model, [] )

        Diff _ diff ->
            case Tasks.processDiff model.time diff model.tasksState of
                Ok newState ->
                    ( { model | tasksState = newState }, [] )

                Err () ->
                    ( { model | notifications = UnknownDiff :: model.notifications }, [] )

        Quit _ ->
            ( model, [ subscribeToTasksUpdates ship ] )


appName : String
appName =
    "tracker"


subscribeToTasksUpdates : Atom -> Urbit.OutMsg
subscribeToTasksUpdates ship =
    Urbit.subscribe
        { ship = ship
        , app = appName
        , path = "/tasks"
        }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout (Font.color Indigo.black80 :: Indigo.paragraphA) <|
        Element.el
            [ width fill
            , height fill
            , inFront <|
                column [ alignRight, alignBottom, padding 24, spacing 12 ]
                    (model.notifications
                        |> List.indexedMap
                            (\index notification ->
                                row
                                    (Border.rounded 8
                                        :: Background.color Indigo.softRed
                                        :: Font.color Indigo.red
                                        :: Indigo.buttons
                                    )
                                    [ textColumn
                                        [ paddingEach { left = 16, top = 8, bottom = 8, right = 0 }
                                        , width (fill |> maximum 400)
                                        , spacing 4
                                        ]
                                        ((case notification of
                                            PokeFailed error ->
                                                "Update failed:"
                                                    :: String.lines error

                                            UnknownDiff ->
                                                [ "Data was received in an unexpected format, which likely means that there was an update to the application and you need to update."
                                                , "Please reload the page to update."
                                                ]
                                         )
                                            |> List.map
                                                (\p ->
                                                    paragraph [ Indigo.buttonsSpacing ]
                                                        [ text p ]
                                                )
                                        )
                                    , Input.button [ alignTop, padding 8, mouseOver [ alpha 0.7 ] ]
                                        { onPress = Just <| NotificationClosed index
                                        , label = Indigo.smallCross
                                        }
                                    ]
                            )
                    )
            ]
            (Element.map TasksMsg <|
                Tasks.view model model.tasksState
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 GotTime
        , Urbit.messages onUrbitMessage GotUrbitMessage
        ]



-- SHIP JS


shipJsStringParser : Parser String
shipJsStringParser =
    Parser.succeed identity
        |. Parser.token "window.ship = \""
        |= (Parser.chompWhile (\c -> Char.isAlpha c || c == '-')
                |> Parser.getChompedString
           )



-- PORTS


port setupUrbitEventSource : String -> Cmd msg


port onUrbitMessage : (Encode.Value -> msg) -> Sub msg
