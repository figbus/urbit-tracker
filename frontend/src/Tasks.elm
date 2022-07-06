module Tasks exposing (..)

import Codec exposing (Codec)
import Date exposing (Date, Unit(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as Attr
import Html.Events
import Indigo
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra
import Random
import SelectList exposing (Position(..), SelectList)
import SelectList.Extra
import SubCmd exposing (SubCmd)
import Table exposing (Table)
import Time exposing (Weekday(..))
import Time.Extra as Time
import Uuid exposing (Uuid)


type alias State =
    { tasks : Tasks
    , habitsNewTaskInput : String
    , dailiesFilter : DailiesFilter
    , dailiesNewTaskInput : String
    , toDosFilter : ToDosFilter
    , toDosNewTaskInput : String
    , openedTaskMenu : Maybe ( Uuid, TaskFormValues )
    }



-- INIT


init : State
init =
    {- TODO
       let
           timeMillis : number
           timeMillis =
               0

           time : Time.Posix
           time =
               Time.millisToPosix timeMillis

              addTask : Task -> Random.Generator Tasks -> Random.Generator Tasks
              addTask task =
                  Random.andThen <|
                      \tasks ->
                          Uuid.uuidGenerator
                              |> Random.map (\id -> updateTasks time (AddTask id task) tasks)

                 seededTasksGenerator : Random.Generator Tasks
                 seededTasksGenerator =
                     Random.constant
                         { habits = Table.empty
                         , dailies = Table.empty
                         , toDos = Table.empty
                         }
                         |> (addTask <|
                                 HabitTask
                                     { title = "Lorem Ipsum"
                                     , notes = ""
                                     , history = []
                                     }
                            )
                         |> (addTask <|
                                 DailyTask
                                     { title = "Every day"
                                     , notes = ""
                                     , startDate = time
                                     , repeatPattern = DailyPattern { every = 1 }
                                     , history = []
                                     }
                            )
                         |> (addTask <|
                                 DailyTask
                                     { title = "Birthday"
                                     , notes = ""
                                     , startDate = Time.millisToPosix 852552000000
                                     , repeatPattern = YearlyPattern { every = 1 }
                                     , history = []
                                     }
                            )
                         |> (addTask <|
                                 ToDoTask
                                     { title = "Lorem Ipsum"
                                     , notes = ""
                                     , dueDate = Nothing
                                     , completedOn = Nothing
                                     }
                            )
                         |> (addTask <|
                                 ToDoTask
                                     { title = "Write this down"
                                     , notes = ""
                                     , dueDate = Just (Time.millisToPosix (timeMillis + 1000))
                                     , completedOn = Nothing
                                     }
                            )
                         |> (addTask <|
                                 ToDoTask
                                     { title = "Look around?"
                                     , notes = ""
                                     , dueDate = Just (Time.millisToPosix (timeMillis + 1000))
                                     , completedOn = Nothing
                                     }
                            )
       in
    -}
    { tasks =
        { habits = Table.empty
        , dailies = Table.empty
        , toDos = Table.empty
        }

    -- Random.step seededTasksGenerator (Random.initialSeed 0) |> Tuple.first
    , habitsNewTaskInput = ""
    , dailiesFilter = All
    , dailiesNewTaskInput = ""
    , toDosFilter = Active
    , toDosNewTaskInput = ""
    , openedTaskMenu = Nothing
    }



-- UPDATE


type Msg
    = Ignore
    | CompleteClicked Uuid
    | UncompleteClicked Uuid
    | HabitsNewTaskInputUpdated String
    | HabitsNewTaskSubmitted
    | DailiesNewTaskInputUpdated String
    | DailiesNewTaskSubmitted
    | DailiesFilterSelected DailiesFilter
    | ToDosNewTaskInputUpdated String
    | ToDosNewTaskInputSubmitted
    | ToDosFilterSelected ToDosFilter
    | NewTaskIdGenerated Task Uuid
    | OpenTaskMenu Uuid Task
    | CloseTaskMenu
    | TaskChanged TaskFormValues
    | TaskSaved
    | TaskDelete


type Effect
    = SendUpdate TrackerUpdate


update :
    { model
        | timeZone : Time.Zone
        , time : Time.Posix
    }
    -> Msg
    -> State
    -> ( State, SubCmd Msg Effect )
update { timeZone, time } msg state =
    case msg of
        Ignore ->
            ( state, SubCmd.none )

        CompleteClicked id ->
            ( state, SubCmd.effect <| SendUpdate <| CompleteTask id )

        UncompleteClicked id ->
            ( state, SubCmd.effect <| SendUpdate <| UncompleteTask id )

        HabitsNewTaskInputUpdated newValue ->
            ( { state | habitsNewTaskInput = newValue }, SubCmd.none )

        HabitsNewTaskSubmitted ->
            let
                newTask : Task
                newTask =
                    HabitTask
                        { order = 1
                        , title = state.habitsNewTaskInput
                        , notes = ""
                        , history = []
                        }
            in
            case state.habitsNewTaskInput of
                "" ->
                    ( state, SubCmd.none )

                _ ->
                    ( { state | habitsNewTaskInput = "" }
                    , Random.generate (NewTaskIdGenerated newTask) Uuid.uuidGenerator
                        |> SubCmd.cmd
                    )

        DailiesNewTaskInputUpdated newValue ->
            ( { state | dailiesNewTaskInput = newValue }, SubCmd.none )

        DailiesNewTaskSubmitted ->
            let
                newTask : Task
                newTask =
                    DailyTask
                        { order = 1
                        , title = state.dailiesNewTaskInput
                        , notes = ""
                        , startDate = time
                        , repeatPattern = DailyPattern { every = 1 }
                        , history = []
                        }
            in
            case state.dailiesNewTaskInput of
                "" ->
                    ( state, SubCmd.none )

                _ ->
                    ( { state | dailiesNewTaskInput = "" }
                    , Random.generate (NewTaskIdGenerated newTask) Uuid.uuidGenerator
                        |> SubCmd.cmd
                    )

        DailiesFilterSelected newFilter ->
            ( { state | dailiesFilter = newFilter }, SubCmd.none )

        ToDosFilterSelected newFilter ->
            ( { state | toDosFilter = newFilter }, SubCmd.none )

        ToDosNewTaskInputUpdated newValue ->
            ( { state | toDosNewTaskInput = newValue }, SubCmd.none )

        ToDosNewTaskInputSubmitted ->
            let
                newTask : Task
                newTask =
                    ToDoTask
                        { order = 1
                        , title = state.toDosNewTaskInput
                        , notes = ""
                        , dueDate = Nothing
                        , completedOn = Nothing
                        }
            in
            case state.toDosNewTaskInput of
                "" ->
                    ( state, SubCmd.none )

                _ ->
                    ( { state | toDosNewTaskInput = "" }
                    , Random.generate (NewTaskIdGenerated newTask) Uuid.uuidGenerator
                        |> SubCmd.cmd
                    )

        NewTaskIdGenerated task id ->
            ( state, SubCmd.effect <| SendUpdate <| AddTask id task )

        OpenTaskMenu id task ->
            ( { state | openedTaskMenu = Just ( id, taskToFormValues timeZone task ) }
            , SubCmd.none
            )

        CloseTaskMenu ->
            ( { state | openedTaskMenu = Nothing }, SubCmd.none )

        TaskChanged newTask ->
            ( case state.openedTaskMenu of
                Just ( id, _ ) ->
                    { state | openedTaskMenu = Just ( id, newTask ) }

                Nothing ->
                    state
            , SubCmd.none
            )

        TaskSaved ->
            let
                maybeParsed : Maybe ( Uuid, TaskUpdate )
                maybeParsed =
                    state.openedTaskMenu
                        |> Maybe.andThen
                            (\( id, form ) ->
                                taskFormToUpdate timeZone form
                                    |> Maybe.map (Tuple.pair id)
                            )
            in
            case maybeParsed of
                Just ( id, taskUpdate ) ->
                    ( { state | openedTaskMenu = Nothing }
                    , SubCmd.effect <| SendUpdate <| UpdateTask id taskUpdate
                    )

                Nothing ->
                    ( state, SubCmd.none )

        TaskDelete ->
            ( { state | openedTaskMenu = Nothing }
            , case state.openedTaskMenu of
                Just ( id, _ ) ->
                    SubCmd.effect <| SendUpdate <| RemoveTask id

                Nothing ->
                    SubCmd.none
            )



-- VIEW


type alias SharedState model =
    { model | timeZone : Time.Zone, time : Time.Posix }


view : SharedState model -> State -> Element Msg
view shared ({ tasks } as state) =
    row
        ((width fill
            :: height fill
            :: padding 48
            :: spacing 48
            :: Indigo.buttons
         )
            |> (state.openedTaskMenu
                    |> Maybe.map
                        (\( _, task ) ->
                            inFront
                                (el
                                    [ width fill
                                    , height fill
                                    , behindContent <|
                                        el
                                            [ width fill
                                            , height fill
                                            , Background.color Indigo.black100
                                            , alpha 0.12
                                            ]
                                            none
                                    ]
                                    (el
                                        [ centerX
                                        , moveDown 32
                                        , width (fill |> maximum 448)
                                        , padding 12
                                        ]
                                        (el
                                            [ Background.color Indigo.white
                                            , padding 24
                                            , width fill
                                            , Border.rounded 12
                                            ]
                                            (taskEditor task)
                                        )
                                    )
                                )
                        )
                    |> Maybe.Extra.cons
               )
        )
        [ habitsColumn shared state
        , dailiesColumn shared state
        , toDosColumn state
        ]


habitsColumn : SharedState model -> State -> Element Msg
habitsColumn { time, timeZone } state =
    let
        today : Int
        today =
            time
                |> Date.fromPosix timeZone
                |> Date.toRataDie
    in
    taskColumn
        { toTypedTask = HabitTask
        , title = "Habits"
        , singular = "Habit"
        , showActiveTasksCount = False
        , icon = el [ padding 4 ] Indigo.smallPlus
        , filterButtons = Nothing
        , tasks = state.tasks.habits
        , isComplete = always False
        , isEnabled = always True
        , streak =
            \habit ->
                let
                    countStreak : Int -> List Time.Posix -> Int
                    countStreak total history =
                        case history of
                            [] ->
                                total

                            t :: r ->
                                if
                                    (t
                                        |> Date.fromPosix timeZone
                                        |> Date.toRataDie
                                    )
                                        == today
                                then
                                    countStreak (total + 1) r

                                else
                                    total
                in
                Just (countStreak 0 habit.history)
        , newTaskInput = state.habitsNewTaskInput
        , onNewTaskInputChange = HabitsNewTaskInputUpdated
        , onNewTaskSubmit = HabitsNewTaskSubmitted
        }


dailiesColumn : SharedState model -> State -> Element Msg
dailiesColumn { timeZone, time } ({ tasks } as state) =
    taskColumn
        { toTypedTask = DailyTask
        , title = "Dailies"
        , singular = "Daily"
        , showActiveTasksCount = True
        , icon = Indigo.check
        , filterButtons =
            Just
                { list =
                    dailiesFilterList
                        |> selectFilterList state.dailiesFilter
                , toLabel = dailiesFilterToString
                , onPress = DailiesFilterSelected
                }
        , tasks =
            case state.dailiesFilter of
                All ->
                    tasks.dailies

                Due ->
                    tasks.dailies
                        |> Table.filter (\_ daily -> isDue timeZone time daily)

                NotDue ->
                    tasks.dailies
                        |> Table.filter (\_ daily -> not <| isDue timeZone time daily)
        , isComplete =
            \task ->
                task.history
                    |> List.head
                    |> Maybe.map (\mostRecent -> isSameDay timeZone mostRecent time)
                    |> Maybe.withDefault False
        , isEnabled = isDue timeZone time
        , streak = always Nothing
        , newTaskInput = state.dailiesNewTaskInput
        , onNewTaskInputChange = DailiesNewTaskInputUpdated
        , onNewTaskSubmit = DailiesNewTaskSubmitted
        }


toDosColumn : State -> Element Msg
toDosColumn state =
    taskColumn
        { toTypedTask = ToDoTask
        , title = "To Do's"
        , singular = "To Do"
        , showActiveTasksCount = True
        , icon = Indigo.check
        , filterButtons =
            Just
                { list = toDosFilterList |> selectFilterList state.toDosFilter
                , toLabel = toDosFilterToString
                , onPress = ToDosFilterSelected
                }
        , tasks =
            state.tasks.toDos
                |> Table.filter
                    (\_ toDo ->
                        case state.toDosFilter of
                            Active ->
                                Maybe.Extra.isNothing toDo.completedOn

                            Scheduled ->
                                Maybe.Extra.isNothing toDo.completedOn
                                    && Maybe.Extra.isJust toDo.dueDate

                            Complete ->
                                Maybe.Extra.isJust toDo.completedOn
                    )
        , isComplete = .completedOn >> Maybe.Extra.isJust
        , isEnabled = always True
        , streak = always Nothing
        , newTaskInput = state.toDosNewTaskInput
        , onNewTaskInputChange = ToDosNewTaskInputUpdated
        , onNewTaskSubmit = ToDosNewTaskInputSubmitted
        }


taskEditor : TaskFormValues -> Element Msg
taskEditor task =
    case task of
        HabitForm habit ->
            habitEditor habit

        DailyForm daily ->
            dailyEditor daily

        ToDoForm toDo ->
            toDoEditor toDo


editor :
    String
    -> ({ task | title : String, notes : String } -> TaskFormValues)
    -> { task | title : String, notes : String }
    -> List (Element Msg)
    -> Element Msg
editor label wrap task children =
    let
        over : Attribute msg
        over =
            mouseOver
                [ Border.shadow
                    { offset = ( 0, 0 )
                    , size = 3
                    , blur = 0
                    , color = Indigo.black20
                    }
                ]

        elementSpacing : number
        elementSpacing =
            20
    in
    column [ width fill, spacing (elementSpacing * 2) ]
        [ column [ width fill, spacing elementSpacing ] <|
            row [ width fill, spacing 8 ]
                [ el (width fill :: Indigo.header1) <| text <| "Edit " ++ label
                , Input.button
                    [ alignTop
                    , paddingXY 16 8
                    , Background.color Indigo.black04
                    , Border.rounded 8
                    , over
                    ]
                    { onPress = Just CloseTaskMenu
                    , label = text "Cancel"
                    }
                , Input.button
                    [ alignTop
                    , paddingXY 16 8
                    , Background.color Indigo.black80
                    , Font.color Indigo.white
                    , Border.rounded 8
                    , over
                    ]
                    { onPress = Just TaskSaved
                    , label = text "Save"
                    }
                ]
                :: Input.text []
                    { onChange = \new -> TaskChanged <| wrap { task | title = new }
                    , text = task.title
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (text "Title")
                    }
                :: Input.multiline [ Indigo.paragraphASpacing ]
                    { onChange = \new -> TaskChanged <| wrap { task | notes = new }
                    , text = task.notes
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (text "Notes")
                    , spellcheck = True
                    }
                :: children
        , Input.button
            [ centerX
            , paddingXY 16 8
            , Background.color Indigo.black04
            , Font.color Indigo.red
            , Border.rounded 8
            , over
            ]
            { onPress = Just TaskDelete
            , label = text <| "Delete this " ++ label
            }
        ]


habitEditor : HabitFormValues -> Element Msg
habitEditor habit =
    editor "Habit" HabitForm habit []


dailyEditor : DailyFormValues -> Element Msg
dailyEditor daily =
    editor "Daily"
        DailyForm
        daily
        [ dateInput
            (\new ->
                case Date.fromIsoString new of
                    Ok _ ->
                        TaskChanged <| DailyForm { daily | startDate = new }

                    Err _ ->
                        Ignore
            )
            daily.startDate
            "Start Date"
        , Input.radioRow [ spacing 16 ]
            { onChange = \new -> TaskChanged <| DailyForm { daily | repeatPattern = new }
            , options =
                [ Input.option DailyPatternForm (viewOption "Daily")
                , Input.option WeeklyPatternForm (viewOption "Weekly")
                , Input.option MonthlyPatternForm (viewOption "Monthly")
                , Input.option YearlyPatternForm (viewOption "Yearly")
                ]
            , selected = Just daily.repeatPattern
            , label =
                Input.labelAbove [ paddingEach { bottom = 8, top = 0, right = 0, left = 0 } ] <|
                    text "Repeats"
            }
        , row [ width fill, spacing 24 ]
            [ el [ width fill ]
                (Html.label [ Attr.style "width" "100%" ]
                    [ Html.div [ Attr.style "margin-bottom" "8px" ] [ Html.text "Repeat Every" ]
                    , Html.input
                        [ Attr.type_ "number"
                        , Attr.min "0"
                        , Attr.value daily.every
                        , Html.Events.onInput <| \new -> TaskChanged <| DailyForm { daily | every = new }
                        , Attr.style "width" "100%"
                        , Attr.style "height" "28px"
                        , Attr.style "padding" "0 8px"
                        ]
                        []
                    ]
                    |> html
                )
            , el [ alignBottom, paddingXY 16 0, Background.color Indigo.black10, height (px 32) ] <|
                el [ centerY ] <|
                    text <|
                        (case daily.repeatPattern of
                            DailyPatternForm ->
                                "Day"

                            WeeklyPatternForm ->
                                "Week"

                            MonthlyPatternForm ->
                                "Month"

                            YearlyPatternForm ->
                                "Year"
                        )
                            ++ (case String.toInt daily.every of
                                    Just 1 ->
                                        ""

                                    _ ->
                                        "s"
                               )
            ]
        , viewRepeatPatternEditor daily
        ]


viewOption : String -> Element msg
viewOption label =
    el [ paddingXY 0 8 ] <|
        text label


viewRepeatPatternEditor : DailyFormValues -> Element Msg
viewRepeatPatternEditor daily =
    case daily.repeatPattern of
        DailyPatternForm ->
            none

        WeeklyPatternForm ->
            let
                checkbox :
                    String
                    -> Bool
                    -> Bool
                    -> (Bool -> DaysOfWeek -> DaysOfWeek)
                    -> Element Msg
                checkbox label isFirst checked onChange =
                    Input.checkbox
                        ([ width fill
                         , paddingXY 0 16
                         , Border.color (ifElse checked Indigo.blue Indigo.black20)
                         , Border.widthEach
                            { top = 1
                            , right = 1
                            , bottom = 1
                            , left = ifElse isFirst 1 0
                            }
                         , inFront <|
                            el [ centerX, centerY ] <|
                                text label
                         ]
                            |> consIf checked (Background.color Indigo.blue)
                            |> consIf checked (Font.color Indigo.white)
                        )
                        { onChange =
                            \new ->
                                TaskChanged <|
                                    DailyForm { daily | daysOfWeek = onChange new daily.daysOfWeek }
                        , icon = \_ -> none
                        , checked = checked
                        , label = Input.labelHidden label
                        }
            in
            column [ width fill, spacing 8 ]
                [ text "Repeat On"
                , row [ width fill ]
                    [ checkbox "Su"
                        True
                        daily.daysOfWeek.sunday
                        (\new daysOfWeek -> { daysOfWeek | sunday = new })
                    , checkbox "Mo"
                        False
                        daily.daysOfWeek.monday
                        (\new daysOfWeek -> { daysOfWeek | monday = new })
                    , checkbox "Tu"
                        False
                        daily.daysOfWeek.tuesday
                        (\new daysOfWeek -> { daysOfWeek | tuesday = new })
                    , checkbox "We"
                        False
                        daily.daysOfWeek.wednesday
                        (\new daysOfWeek -> { daysOfWeek | wednesday = new })
                    , checkbox "Th"
                        False
                        daily.daysOfWeek.thursday
                        (\new daysOfWeek -> { daysOfWeek | thursday = new })
                    , checkbox "Fr"
                        False
                        daily.daysOfWeek.friday
                        (\new daysOfWeek -> { daysOfWeek | friday = new })
                    , checkbox "Sa"
                        False
                        daily.daysOfWeek.saturday
                        (\new daysOfWeek -> { daysOfWeek | saturday = new })
                    ]
                ]

        MonthlyPatternForm ->
            Input.radioRow [ spacing 16 ]
                { onChange = \new -> TaskChanged <| DailyForm { daily | repeatOn = new }
                , options =
                    [ Input.option DayOfMonth (viewOption "Day of the Month")
                    , Input.option DayOfWeek (viewOption "Day of the Week")
                    ]
                , selected = Just daily.repeatOn
                , label =
                    Input.labelAbove [ paddingEach { bottom = 8, top = 0, right = 0, left = 0 } ] <|
                        text "Repeat On"
                }

        YearlyPatternForm ->
            none


toDoEditor : ToDoFormValues -> Element Msg
toDoEditor toDo =
    editor "To Do"
        ToDoForm
        toDo
        [ dateInput
            (\new -> TaskChanged <| ToDoForm { toDo | dueDate = new })
            toDo.dueDate
            "Due Date"
        ]


dateInput : (String -> msg) -> String -> String -> Element msg
dateInput onChange date label =
    Html.label []
        [ Html.div [ Attr.style "margin-bottom" "8px" ] [ Html.text label ]
        , Html.input
            [ Attr.type_ "date"
            , Html.Events.onInput onChange
            , Attr.value date
            ]
            []
        ]
        |> html
        |> el []


type alias FilterButtons a =
    { list : SelectList a
    , toLabel : a -> String
    , onPress : a -> Msg
    }


type alias TaskCommon task =
    { task
        | order : Int
        , title : String
        , notes : String
    }


taskColumn :
    { toTypedTask : TaskCommon task -> Task
    , title : String
    , singular : String
    , showActiveTasksCount : Bool
    , icon : Element Msg
    , filterButtons : Maybe (FilterButtons a)
    , tasks : Table (TaskCommon task)
    , isComplete : TaskCommon task -> Bool
    , isEnabled : TaskCommon task -> Bool
    , streak : TaskCommon task -> Maybe Int
    , newTaskInput : String
    , onNewTaskInputChange : String -> Msg
    , onNewTaskSubmit : Msg
    }
    -> Element Msg
taskColumn config =
    let
        activeTasksCount : Int
        activeTasksCount =
            config.tasks
                |> Table.filter
                    (\_ task ->
                        config.isEnabled task
                            && not (config.isComplete task)
                    )
                |> Table.size

        viewActiveTasksCount : Element msg
        viewActiveTasksCount =
            if activeTasksCount > 0 && config.showActiveTasksCount then
                el
                    (alignBottom
                        :: width (px 24)
                        :: height (px 24)
                        :: Background.color Indigo.blue
                        :: Font.color Indigo.white
                        :: Border.rounded 999
                        :: Indigo.smallLabelBold
                    )
                    (el [ centerX, centerY ] <| text <| String.fromInt activeTasksCount)

            else
                none
    in
    column [ alignTop, width fill, spacing 12 ]
        [ row [ width fill, spacing 8 ]
            [ el Indigo.header1 <| text config.title
            , viewActiveTasksCount
            , config.filterButtons |> Maybe.map viewFilterButtons |> Maybe.withDefault none
            ]
        , column
            [ width fill
            , padding 8
            , spacing 12
            , Background.color Indigo.black10
            , Border.rounded 12
            ]
            [ Input.text
                (paddingXY 16 ((32 - 14) // 2)
                    :: Background.color Indigo.white
                    :: Border.rounded 8
                    :: Border.width 0
                    :: onEnter config.onNewTaskSubmit
                    :: Indigo.buttons
                )
                { onChange = config.onNewTaskInputChange
                , text = config.newTaskInput
                , placeholder =
                    Just <|
                        Input.placeholder [ Font.color Indigo.black40 ] <|
                            text ("Add a " ++ config.singular)
                , label = Input.labelHidden config.singular
                }
            , if Table.isEmpty config.tasks then
                none

              else
                config.tasks
                    |> Table.toList
                    |> List.sortBy (Tuple.second >> .order)
                    |> List.map
                        (viewTask
                            config.toTypedTask
                            config.icon
                            config.isComplete
                            config.isEnabled
                            config.streak
                        )
                    |> column [ width fill, spacing 8 ]
            ]
        ]


viewFilterButtons : FilterButtons a -> Element Msg
viewFilterButtons { list, toLabel, onPress } =
    let
        filterButton : Position -> SelectList a -> Element Msg
        filterButton position selectList =
            let
                buttonElement : a
                buttonElement =
                    SelectList.selected selectList
            in
            el
                (padding 4
                    :: mouseOver [ Font.color Indigo.black80 ]
                    :: pointer
                    :: (case position of
                            Selected ->
                                [ Font.color Indigo.black80
                                , Font.underline
                                ]

                            _ ->
                                [ Events.onClick (onPress buttonElement) ]
                       )
                )
                (text (toLabel buttonElement))
    in
    row (alignBottom :: alignRight :: Font.color Indigo.black60 :: Indigo.buttons) <|
        SelectList.selectedMap filterButton list


viewTask :
    (TaskCommon task -> Task)
    -> Element Msg
    -> (TaskCommon task -> Bool)
    -> (TaskCommon task -> Bool)
    -> (TaskCommon task -> Maybe Int)
    -> ( Uuid, TaskCommon task )
    -> Element Msg
viewTask toTypedTask icon isComplete isEnabled maybeStreak ( id, task ) =
    row
        [ width fill
        , Background.color Indigo.white
        , mouseOver
            [ Border.shadow
                { offset = ( 0, 0 )
                , size = 3
                , blur = 0
                , color = Indigo.black20
                }
            ]
        , Border.rounded 12
        ]
        [ el [ alignTop, padding 8 ] <|
            if isEnabled task then
                Input.button
                    (Border.rounded 4
                        :: (if isComplete task then
                                [ Background.color Indigo.black100
                                , Font.color Indigo.white
                                , mouseOver [ Background.color Indigo.black60 ]
                                , pointer
                                ]

                            else
                                [ Background.color Indigo.black20
                                , Font.color Indigo.black20
                                , mouseOver [ Background.color Indigo.black40 ]
                                ]
                           )
                    )
                    { onPress =
                        Just <|
                            if isComplete task then
                                UncompleteClicked id

                            else
                                CompleteClicked id
                    , label = icon
                    }

            else
                el
                    (Border.rounded 4
                        :: (if isComplete task then
                                [ Background.color Indigo.black40
                                , Font.color Indigo.black20
                                ]

                            else
                                [ Background.color Indigo.black40
                                , Font.color Indigo.black40
                                , mouseDown [ Background.color Indigo.black80 ]
                                , mouseOver [ Background.color Indigo.black40 ]
                                ]
                           )
                    )
                    icon
        , Input.button [ width fill, paddingXY 4 ((40 - 14) // 2) ]
            { onPress = Just <| OpenTaskMenu id (toTypedTask task)
            , label =
                column [ width fill, spacing 18 ]
                    ([ row [ width fill, spacing 8 ]
                        ([ paragraph [ width fill, Indigo.paragraphASpacing ] [ text task.title ] ]
                            |> maybeMapAppend
                                (\streak ->
                                    el [ alignTop, Font.color Indigo.black60, Font.italic ] <|
                                        text (String.fromInt streak)
                                )
                                (maybeStreak task)
                        )
                     ]
                        |> appendIf (not <| String.isEmpty task.notes)
                            [ textColumn
                                [ width fill
                                , Font.color Indigo.black60
                                , Indigo.paragraphASpacing
                                ]
                                (task.notes
                                    |> String.lines
                                    |> List.map
                                        (\line ->
                                            paragraph [ Indigo.paragraphASpacing ] [ text line ]
                                        )
                                )
                            ]
                    )
            }
        , Input.button
            [ alignTop
            , padding 8
            , Border.roundEach { topRight = 12, bottomRight = 12, topLeft = 0, bottomLeft = 0 }
            , Font.color Indigo.black40
            , mouseOver [ Font.color Indigo.black60 ]
            ]
            { onPress = Just <| OpenTaskMenu id (toTypedTask task)
            , label = Indigo.optionsA
            }
        ]



-- TASKS


type alias Tasks =
    { habits : Table Habit
    , dailies : Table Daily
    , toDos : Table ToDo
    }


decoder : Decoder Tasks
decoder =
    Decode.map3 Tasks
        (Decode.field "habits"
            (Decode.dict (Codec.decoder habitCodec))
            |> Decode.map Table.fromDict
        )
        (Decode.field "dailies"
            (Decode.dict (Codec.decoder dailyCodec))
            |> Decode.map Table.fromDict
        )
        (Decode.field "todos"
            (Decode.dict (Codec.decoder toDoCodec))
            |> Decode.map Table.fromDict
        )



-- TASK


type Task
    = HabitTask Habit
    | DailyTask Daily
    | ToDoTask ToDo



-- TASK FORM VALUES


type TaskFormValues
    = HabitForm HabitFormValues
    | DailyForm DailyFormValues
    | ToDoForm ToDoFormValues


taskToFormValues : Time.Zone -> Task -> TaskFormValues
taskToFormValues timeZone task =
    case task of
        HabitTask habit ->
            HabitForm
                { title = habit.title
                , notes = habit.notes
                }

        DailyTask daily ->
            let
                allWeekEnabled : DaysOfWeek
                allWeekEnabled =
                    DaysOfWeek True True True True True True True

                { repeatPattern, every, daysOfWeek, repeatOn } =
                    case daily.repeatPattern of
                        DailyPattern p ->
                            { repeatPattern = DailyPatternForm
                            , every = String.fromInt p.every
                            , daysOfWeek = allWeekEnabled
                            , repeatOn = DayOfMonth
                            }

                        WeeklyPattern p ->
                            { repeatPattern = WeeklyPatternForm
                            , every = String.fromInt p.every
                            , daysOfWeek = p.daysOfWeek
                            , repeatOn = DayOfMonth
                            }

                        MonthlyPattern p ->
                            { repeatPattern = MonthlyPatternForm
                            , every = String.fromInt p.every
                            , daysOfWeek = allWeekEnabled
                            , repeatOn = p.repeatOn
                            }

                        YearlyPattern p ->
                            { repeatPattern = YearlyPatternForm
                            , every = String.fromInt p.every
                            , daysOfWeek = allWeekEnabled
                            , repeatOn = DayOfMonth
                            }
            in
            DailyForm
                { title = daily.title
                , notes = daily.notes
                , startDate = timeToValue timeZone daily.startDate
                , repeatPattern = repeatPattern
                , every = every
                , daysOfWeek = daysOfWeek
                , repeatOn = repeatOn
                }

        ToDoTask toDo ->
            ToDoForm
                { title = toDo.title
                , notes = toDo.notes
                , dueDate =
                    toDo.dueDate
                        |> Maybe.map (timeToValue timeZone)
                        |> Maybe.withDefault ""
                }


timeToValue : Time.Zone -> Time.Posix -> String
timeToValue timeZone =
    Date.fromPosix timeZone
        >> Date.toIsoString


timeParser : Time.Zone -> String -> Maybe Time.Posix
timeParser timeZone =
    Date.fromIsoString
        >> Result.toMaybe
        >> Maybe.map (dateToPosix timeZone)


dateToPosix : Time.Zone -> Date -> Time.Posix
dateToPosix zone date =
    Time.partsToPosix zone
        { year = Date.year date
        , month = Date.month date
        , day = Date.day date
        , hour = 0
        , minute = 0
        , second = 0
        , millisecond = 0
        }


taskFormToUpdate : Time.Zone -> TaskFormValues -> Maybe TaskUpdate
taskFormToUpdate timeZone form =
    case form of
        HabitForm habit ->
            Just <|
                HabitTaskUpdate
                    { title = Just habit.title
                    , notes = Just habit.notes
                    }

        DailyForm daily ->
            Maybe.map2
                (\startDate repeatPattern ->
                    DailyTaskUpdate
                        { title = Just daily.title
                        , notes = Just daily.notes
                        , startDate = Just startDate
                        , repeatPattern = Just repeatPattern
                        }
                )
                (timeParser timeZone daily.startDate)
                (case daily.repeatPattern of
                    DailyPatternForm ->
                        Maybe.map DailyPattern <|
                            Maybe.map DailyPatternData
                                (String.toInt daily.every)

                    WeeklyPatternForm ->
                        Maybe.map WeeklyPattern <|
                            Maybe.map2 WeeklyPatternData
                                (String.toInt daily.every)
                                (Just daily.daysOfWeek)

                    MonthlyPatternForm ->
                        Maybe.map MonthlyPattern <|
                            Maybe.map2 MonthlyPatternData
                                (String.toInt daily.every)
                                (Just daily.repeatOn)

                    YearlyPatternForm ->
                        Maybe.map YearlyPattern <|
                            Maybe.map YearlyPatternData
                                (String.toInt daily.every)
                )

        ToDoForm toDo ->
            Maybe.map
                (\dueDate ->
                    ToDoTaskUpdate
                        { title = Just toDo.title
                        , notes = Just toDo.notes
                        , dueDate = Just dueDate
                        }
                )
                (case toDo.dueDate of
                    "" ->
                        Just Nothing

                    dueDate ->
                        timeParser timeZone dueDate
                            |> Maybe.map Just
                )



-- HABIT


type alias Habit =
    { order : Int
    , title : String
    , notes : String
    , history : List Time.Posix
    }


type alias HabitFormValues =
    { title : String
    , notes : String
    }


type alias HabitUpdate =
    { title : Maybe String
    , notes : Maybe String
    }


habitCodec : Codec Habit
habitCodec =
    Codec.object Habit
        |> Codec.field "order" .order Codec.int
        |> Codec.field "title" .title Codec.string
        |> Codec.field "notes" .notes Codec.string
        |> Codec.field "history" .history (Codec.list timeCodec)
        |> Codec.buildObject


habitUpdateCodec : Codec HabitUpdate
habitUpdateCodec =
    Codec.object HabitUpdate
        |> Codec.maybeField "title" .title Codec.string
        |> Codec.maybeField "notes" .notes Codec.string
        |> Codec.buildObject



-- DAILY


type alias Daily =
    { order : Int
    , title : String
    , notes : String
    , startDate : Time.Posix
    , repeatPattern : RepeatPattern
    , history : List Time.Posix
    }


type alias DailyFormValues =
    { title : String
    , notes : String
    , startDate : String
    , repeatPattern : RepeatPatternFormValue
    , every : String
    , daysOfWeek : DaysOfWeek
    , repeatOn : MonthlyRepeatOn
    }


type RepeatPatternFormValue
    = DailyPatternForm
    | WeeklyPatternForm
    | MonthlyPatternForm
    | YearlyPatternForm


type alias DailyPatternFormValues =
    { every : String
    }


type alias WeeklyPatternFormValues =
    { every : String
    , daysOfWeek : DaysOfWeek
    }


type alias MonthlyPatternFormValues =
    { every : String
    , repeatOn : MonthlyRepeatOn
    }


type alias YearlyPatternFormValues =
    { every : String
    }


type alias DailyUpdate =
    { title : Maybe String
    , notes : Maybe String
    , startDate : Maybe Time.Posix
    , repeatPattern : Maybe RepeatPattern
    }


type RepeatPattern
    = DailyPattern DailyPatternData
    | WeeklyPattern WeeklyPatternData
    | MonthlyPattern MonthlyPatternData
    | YearlyPattern YearlyPatternData


type alias DailyPatternData =
    { every : Int
    }


type alias WeeklyPatternData =
    { every : Int
    , daysOfWeek : DaysOfWeek
    }


type alias DaysOfWeek =
    { sunday : Bool
    , monday : Bool
    , tuesday : Bool
    , wednesday : Bool
    , thursday : Bool
    , friday : Bool
    , saturday : Bool
    }


type alias MonthlyPatternData =
    { every : Int
    , repeatOn : MonthlyRepeatOn
    }


type MonthlyRepeatOn
    = DayOfMonth
    | DayOfWeek


type alias YearlyPatternData =
    { every : Int
    }


type DailiesFilter
    = All
    | Due
    | NotDue


dailyCodec : Codec Daily
dailyCodec =
    Codec.object Daily
        |> Codec.field "order" .order Codec.int
        |> Codec.field "title" .title Codec.string
        |> Codec.field "notes" .notes Codec.string
        |> Codec.field "start-date" .startDate timeCodec
        |> Codec.field "repeat-pattern" .repeatPattern repeatPatternCodec
        |> Codec.field "history" .history (Codec.list timeCodec)
        |> Codec.buildObject


dailyUpdateCodec : Codec DailyUpdate
dailyUpdateCodec =
    Codec.object DailyUpdate
        |> Codec.maybeField "title" .title Codec.string
        |> Codec.maybeField "notes" .notes Codec.string
        |> Codec.maybeField "start-date" .startDate timeCodec
        |> Codec.maybeField "repeat-pattern" .repeatPattern repeatPatternCodec
        |> Codec.buildObject


repeatPatternCodec : Codec RepeatPattern
repeatPatternCodec =
    Codec.custom
        (\daily weekly monthly yearly value ->
            case value of
                DailyPattern d ->
                    daily d

                WeeklyPattern d ->
                    weekly d

                MonthlyPattern d ->
                    monthly d

                YearlyPattern d ->
                    yearly d
        )
        |> Codec.variant1 "daily"
            DailyPattern
            (Codec.object DailyPatternData
                |> Codec.field "every" .every Codec.int
                |> Codec.buildObject
            )
        |> Codec.variant1 "weekly"
            WeeklyPattern
            (Codec.object WeeklyPatternData
                |> Codec.field "every" .every Codec.int
                |> Codec.field "days-of-week" .daysOfWeek daysOfWeekCodec
                |> Codec.buildObject
            )
        |> Codec.variant1 "monthly"
            MonthlyPattern
            (Codec.object MonthlyPatternData
                |> Codec.field "every" .every Codec.int
                |> Codec.field "repeat-on" .repeatOn monthlyRepeatOnCodec
                |> Codec.buildObject
            )
        |> Codec.variant1 "yearly"
            YearlyPattern
            (Codec.object YearlyPatternData
                |> Codec.field "every" .every Codec.int
                |> Codec.buildObject
            )
        |> Codec.buildCustom


daysOfWeekCodec : Codec DaysOfWeek
daysOfWeekCodec =
    Codec.object DaysOfWeek
        |> Codec.field "sunday" .sunday Codec.bool
        |> Codec.field "monday" .monday Codec.bool
        |> Codec.field "tuesday" .tuesday Codec.bool
        |> Codec.field "wednesday" .wednesday Codec.bool
        |> Codec.field "thursday" .thursday Codec.bool
        |> Codec.field "friday" .friday Codec.bool
        |> Codec.field "saturday" .saturday Codec.bool
        |> Codec.buildObject


monthlyRepeatOnCodec : Codec MonthlyRepeatOn
monthlyRepeatOnCodec =
    Codec.custom0
        (\m w v ->
            case v of
                DayOfMonth ->
                    m

                DayOfWeek ->
                    w
        )
        |> Codec.variant0 "day-of-month" DayOfMonth
        |> Codec.variant0 "day-of-week" DayOfWeek
        |> Codec.buildCustom0


dailiesFilterList : SelectList DailiesFilter
dailiesFilterList =
    SelectList.fromLists [] All [ Due, NotDue ]


dailiesFilterToString : DailiesFilter -> String
dailiesFilterToString dailiesFilter =
    case dailiesFilter of
        All ->
            "All"

        Due ->
            "Due"

        NotDue ->
            "Not Due"


isDue :
    Time.Zone
    -> Time.Posix
    -> { task | startDate : Time.Posix, repeatPattern : RepeatPattern }
    -> Bool
isDue timeZone time task =
    let
        date : Date
        date =
            Date.fromPosix timeZone time

        rataDie : Int
        rataDie =
            Date.toRataDie date

        startDate : Date
        startDate =
            Date.fromPosix timeZone task.startDate

        startRataDie : Int
        startRataDie =
            Date.toRataDie startDate

        isDayOfWeekEnabled : DaysOfWeek -> Bool
        isDayOfWeekEnabled daysOfWeek =
            case Date.weekday date of
                Mon ->
                    daysOfWeek.monday

                Tue ->
                    daysOfWeek.tuesday

                Wed ->
                    daysOfWeek.wednesday

                Thu ->
                    daysOfWeek.thursday

                Fri ->
                    daysOfWeek.friday

                Sat ->
                    daysOfWeek.saturday

                Sun ->
                    daysOfWeek.sunday
    in
    case compare rataDie startRataDie of
        LT ->
            False

        EQ ->
            case task.repeatPattern of
                DailyPattern _ ->
                    True

                WeeklyPattern { daysOfWeek } ->
                    isDayOfWeekEnabled daysOfWeek

                MonthlyPattern _ ->
                    True

                YearlyPattern _ ->
                    True

        GT ->
            case task.repeatPattern of
                DailyPattern { every } ->
                    case rataDie - startRataDie |> modBy every of
                        0 ->
                            True

                        _ ->
                            False

                WeeklyPattern { every, daysOfWeek } ->
                    let
                        absoluteWeek : Int
                        absoluteWeek =
                            rataDieToAbsoluteWeekNumber rataDie

                        startAbsoluteWeek : Int
                        startAbsoluteWeek =
                            rataDieToAbsoluteWeekNumber startRataDie
                    in
                    case absoluteWeek - startAbsoluteWeek |> modBy every of
                        0 ->
                            isDayOfWeekEnabled daysOfWeek

                        _ ->
                            False

                MonthlyPattern { every, repeatOn } ->
                    let
                        absoluteMonth : Int
                        absoluteMonth =
                            toAbsoluteMonth date

                        startAbsoluteMonth : Int
                        startAbsoluteMonth =
                            toAbsoluteMonth startDate
                    in
                    case absoluteMonth - startAbsoluteMonth |> modBy every of
                        0 ->
                            case repeatOn of
                                DayOfMonth ->
                                    Date.day date == Date.day startDate

                                DayOfWeek ->
                                    (Date.day date + toMonthWeekdayOffset date)
                                        == (Date.day startDate + toMonthWeekdayOffset startDate)

                        _ ->
                            False

                YearlyPattern { every } ->
                    case Date.year date - Date.year startDate |> modBy every of
                        0 ->
                            Date.ordinalDay date == Date.ordinalDay startDate

                        _ ->
                            False


rataDieToAbsoluteWeekNumber : Int -> Int
rataDieToAbsoluteWeekNumber rataDie =
    floor (toFloat rataDie / 7) + 1


toAbsoluteMonth : Date -> Int
toAbsoluteMonth date =
    let
        year : Int
        year =
            Date.year date

        month : Int
        month =
            Date.monthNumber date
    in
    (year - 1) * 12 + month


toMonthWeekdayOffset : Date -> Int
toMonthWeekdayOffset date =
    case Date.fromCalendarDate (Date.year date) (Date.month date) 1 |> Date.weekday of
        Sun ->
            0

        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6



-- TODO


type alias ToDo =
    { order : Int
    , title : String
    , notes : String
    , dueDate : Maybe Time.Posix
    , completedOn : Maybe Time.Posix
    }


type alias ToDoFormValues =
    { title : String
    , notes : String
    , dueDate : String
    }


type alias ToDoUpdate =
    { title : Maybe String
    , notes : Maybe String
    , dueDate : Maybe (Maybe Time.Posix)
    }


type ToDosFilter
    = Active
    | Scheduled
    | Complete


toDoCodec : Codec ToDo
toDoCodec =
    Codec.object ToDo
        |> Codec.field "order" .order Codec.int
        |> Codec.field "title" .title Codec.string
        |> Codec.field "notes" .notes Codec.string
        |> Codec.field "due-date" .dueDate (Codec.maybe timeCodec)
        |> Codec.field "completed-on" .completedOn (Codec.maybe timeCodec)
        |> Codec.buildObject


toDoUpdateCodec : Codec ToDoUpdate
toDoUpdateCodec =
    Codec.object ToDoUpdate
        |> Codec.maybeField "title" .title Codec.string
        |> Codec.maybeField "notes" .notes Codec.string
        |> Codec.maybeField "due-date" .dueDate (Codec.maybe timeCodec)
        |> Codec.buildObject


toDosFilterList : SelectList ToDosFilter
toDosFilterList =
    SelectList.fromLists [] Active [ Scheduled, Complete ]


toDosFilterToString : ToDosFilter -> String
toDosFilterToString toDosFilter =
    case toDosFilter of
        Active ->
            "Active"

        Scheduled ->
            "Scheduled"

        Complete ->
            "Complete"



-- TRACKER UPDATE


type TrackerUpdate
    = AddTask Uuid Task
    | UpdateTask Uuid TaskUpdate
    | RemoveTask Uuid
    | CompleteTask Uuid
    | UncompleteTask Uuid


type TaskUpdate
    = HabitTaskUpdate HabitUpdate
    | DailyTaskUpdate DailyUpdate
    | ToDoTaskUpdate ToDoUpdate


updateVal : Maybe a -> a -> a
updateVal maybe old =
    case maybe of
        Just value ->
            value

        Nothing ->
            old


trackerUpdateCodec : Codec TrackerUpdate
trackerUpdateCodec =
    let
        codecFromMaybe : String -> Maybe a -> Codec a
        codecFromMaybe msg maybe =
            case maybe of
                Just value ->
                    Codec.succeed value

                Nothing ->
                    Codec.fail msg

        uuidCodec : Codec Uuid
        uuidCodec =
            Codec.string
                |> Codec.andThen
                    (Uuid.fromString >> codecFromMaybe "Not a valid UUID")
                    Uuid.toString
    in
    Codec.custom
        (\add remove update_ complete uncomplete v ->
            case v of
                AddTask i t ->
                    add i t

                RemoveTask i ->
                    remove i

                UpdateTask i u ->
                    update_ i u

                CompleteTask i ->
                    complete i

                UncompleteTask i ->
                    uncomplete i
        )
        |> Codec.variant2 "add-task" AddTask ( "uuid", uuidCodec ) ( "task", taskCodec )
        |> Codec.variant1 "remove-task" RemoveTask uuidCodec
        |> Codec.variant2 "update-task" UpdateTask ( "uuid", uuidCodec ) ( "update", taskUpdateCodec )
        |> Codec.variant1 "complete-task" CompleteTask uuidCodec
        |> Codec.variant1 "uncomplete-task" UncompleteTask uuidCodec
        |> Codec.buildCustom


taskCodec : Codec Task
taskCodec =
    Codec.custom
        (\habit daily toDo value ->
            case value of
                HabitTask h ->
                    habit h

                DailyTask d ->
                    daily d

                ToDoTask t ->
                    toDo t
        )
        |> Codec.variant1 "habit" HabitTask habitCodec
        |> Codec.variant1 "daily" DailyTask dailyCodec
        |> Codec.variant1 "todo" ToDoTask toDoCodec
        |> Codec.buildCustom


taskUpdateCodec : Codec TaskUpdate
taskUpdateCodec =
    Codec.custom
        (\habit daily toDo value ->
            case value of
                HabitTaskUpdate h ->
                    habit h

                DailyTaskUpdate d ->
                    daily d

                ToDoTaskUpdate t ->
                    toDo t
        )
        |> Codec.variant1 "habit" HabitTaskUpdate habitUpdateCodec
        |> Codec.variant1 "daily" DailyTaskUpdate dailyUpdateCodec
        |> Codec.variant1 "todo" ToDoTaskUpdate toDoUpdateCodec
        |> Codec.buildCustom


processDiff : Time.Posix -> Codec.Value -> State -> Result () State
processDiff time diff state =
    case Codec.decodeValue trackerUpdateCodec diff of
        Ok trackerUpdate ->
            Ok { state | tasks = updateTasks time trackerUpdate state.tasks }

        Err _ ->
            case Decode.decodeValue decoder diff of
                Ok tasks ->
                    Ok { state | tasks = tasks }

                Err _ ->
                    Err ()


updateTasks : Time.Posix -> TrackerUpdate -> Tasks -> Tasks
updateTasks time event tasks =
    case event of
        AddTask id (HabitTask task) ->
            { tasks | habits = addTask id task tasks.habits }

        AddTask id (DailyTask task) ->
            { tasks | dailies = addTask id task tasks.dailies }

        AddTask id (ToDoTask task) ->
            { tasks | toDos = addTask id task tasks.toDos }

        UpdateTask id (HabitTaskUpdate taskUpdate) ->
            { tasks
                | habits =
                    Table.update id
                        (Maybe.map
                            (\habit ->
                                { habit
                                    | title = updateVal taskUpdate.title habit.title
                                    , notes = updateVal taskUpdate.notes habit.notes
                                }
                            )
                        )
                        tasks.habits
            }

        UpdateTask id (DailyTaskUpdate taskUpdate) ->
            { tasks
                | dailies =
                    Table.update id
                        (Maybe.map
                            (\daily ->
                                { daily
                                    | title = updateVal taskUpdate.title daily.title
                                    , notes = updateVal taskUpdate.notes daily.notes
                                    , startDate = updateVal taskUpdate.startDate daily.startDate
                                    , repeatPattern = updateVal taskUpdate.repeatPattern daily.repeatPattern
                                }
                            )
                        )
                        tasks.dailies
            }

        UpdateTask id (ToDoTaskUpdate taskUpdate) ->
            { tasks
                | toDos =
                    Table.update id
                        (Maybe.map
                            (\toDo ->
                                { toDo
                                    | title = updateVal taskUpdate.title toDo.title
                                    , notes = updateVal taskUpdate.notes toDo.notes
                                    , dueDate = updateVal taskUpdate.dueDate toDo.dueDate
                                }
                            )
                        )
                        tasks.toDos
            }

        RemoveTask id ->
            { habits = removeTask id tasks.habits
            , dailies = removeTask id tasks.dailies
            , toDos = removeTask id tasks.toDos
            }

        CompleteTask id ->
            let
                addTimeToHistory :
                    Table { task | history : List Time.Posix }
                    -> Table { task | history : List Time.Posix }
                addTimeToHistory taskSet =
                    updateTable id (\task -> { task | history = time :: task.history }) taskSet
            in
            { tasks
                | habits = addTimeToHistory tasks.habits
                , dailies = addTimeToHistory tasks.dailies
                , toDos = updateTable id (\task -> { task | completedOn = Just time }) tasks.toDos
            }

        UncompleteTask id ->
            let
                undoMostRecent :
                    Table { task | history : List b }
                    -> Table { task | history : List b }
                undoMostRecent =
                    updateTable id
                        (\task ->
                            { task
                                | history =
                                    List.tail task.history
                                        |> Maybe.withDefault []
                            }
                        )
            in
            { tasks
                | habits = undoMostRecent tasks.habits
                , dailies = undoMostRecent tasks.dailies
                , toDos = updateTable id (\task -> { task | completedOn = Nothing }) tasks.toDos
            }


updateTable : Uuid -> (a -> a) -> Table a -> Table a
updateTable id alter table =
    Table.update id (Maybe.map alter) table


addTask : Uuid -> TaskCommon task -> Table (TaskCommon task) -> Table (TaskCommon task)
addTask id newTask table =
    table
        |> Table.map (\_ t -> { t | order = t.order + 1 })
        |> Table.insert id newTask


removeTask : Uuid -> Table (TaskCommon task) -> Table (TaskCommon task)
removeTask id tasks =
    case Table.get id tasks of
        Just removedHabit ->
            tasks
                |> Table.remove id
                |> Table.map
                    (\_ task ->
                        if task.order < removedHabit.order then
                            task

                        else
                            { task | order = task.order - 1 }
                    )

        Nothing ->
            tasks



-- FILTER LIST HELPERS


selectFilterList : a -> SelectList a -> SelectList a
selectFilterList a list =
    SelectList.Extra.selectElement a list
        |> Maybe.withDefault list



-- DATE


isSameDay : Time.Zone -> Time.Posix -> Time.Posix -> Bool
isSameDay timeZone a b =
    Date.fromPosix timeZone a == Date.fromPosix timeZone b



-- HELPERS


timeCodec : Codec Time.Posix
timeCodec =
    Codec.int
        |> Codec.map Time.millisToPosix Time.posixToMillis


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


consIf : Bool -> a -> List a -> List a
consIf predicate item list =
    if predicate then
        item :: list

    else
        list


appendIf : Bool -> List a -> List a -> List a
appendIf predicate new list =
    if predicate then
        list ++ new

    else
        list


ifElse : Bool -> a -> a -> a
ifElse predicate ifTrue ifFalse =
    if predicate then
        ifTrue

    else
        ifFalse


maybeMapCons : (a -> b) -> Maybe a -> List b -> List b
maybeMapCons f maybe =
    Maybe.Extra.cons (Maybe.map f maybe)


maybeMapAppend : (a -> b) -> Maybe a -> List b -> List b
maybeMapAppend f maybe list =
    case Maybe.map f maybe of
        Just value ->
            list ++ [ value ]

        Nothing ->
            list
