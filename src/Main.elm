module Main exposing (main)

import Basics exposing (toFloat)
import Browser
import Css exposing (..)
import FeatherIcons
import Html.Events
import Html.Styled exposing (Html, button, div, h1, input, li, section, span, text, ul)
import Html.Styled.Attributes exposing (css, name, placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type PomodoroState
    = Work
    | Break
    | LongBreak


type alias Timer =
    { state : PomodoroState
    , completedPomodoros : Int
    , completedSets : Int
    , milliSecLeft : Int
    , paused : Bool
    }


type alias Task =
    { id : Int
    , name : String
    , completed : Bool
    }


type alias Model =
    { timer : Timer
    , inputText : String
    , tasks : List Task
    }


defaultTasks : List Task
defaultTasks =
    [ { id = 1, name = "Drink water", completed = True }
    , { id = 2, name = "Make porridge", completed = True }
    , { id = 3, name = "Journaling", completed = False }
    , { id = 4, name = "Yoga", completed = False }
    ]


defaultPomodoroState : Timer
defaultPomodoroState =
    { state = Work
    , completedPomodoros = 0
    , completedSets = 0
    , milliSecLeft = 2000
    , paused = False
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { timer = defaultPomodoroState, inputText = "", tasks = defaultTasks }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time.Posix
    | ChangePomoState
    | TogglePauseTimer
    | AddTask
    | RemoveTask Int
    | ToggleTask Int
    | ChangeInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick _ ->
            if model.timer.paused then
                -- do nothing
                ( model, Cmd.none )

            else if model.timer.milliSecLeft < 1000 then
                update ChangePomoState model

            else
                ( { model | timer = tick model.timer }, Cmd.none )

        ChangePomoState ->
            case model.timer.state of
                Work ->
                    let
                        prevTimerState =
                            model.timer

                        newTimerState =
                            { prevTimerState | completedPomodoros = prevTimerState.completedPomodoros + 1 }
                    in
                    if newTimerState.completedPomodoros == 4 then
                        ( { model | timer = setLongBreakState model.timer }, Cmd.none )

                    else
                        ( { model | timer = setBreakState model.timer }, Cmd.none )

                Break ->
                    ( { model | timer = setWorkState model.timer }, Cmd.none )

                LongBreak ->
                    ( { model | timer = setWorkState model.timer }, Cmd.none )

        TogglePauseTimer ->
            let
                prevTimerState =
                    model.timer

                newTimerState =
                    { prevTimerState | paused = not prevTimerState.paused }
            in
            ( { model | timer = newTimerState }, Cmd.none )

        AddTask ->
            ( { model
                | tasks = addTask model.inputText model.tasks
                , inputText = ""
              }
            , Cmd.none
            )

        RemoveTask index ->
            ( { model | tasks = removeTask index model.tasks }, Cmd.none )

        ToggleTask index ->
            ( { model | tasks = toggleAtIndex index model.tasks }, Cmd.none )

        ChangeInput input ->
            ( { model | inputText = input }, Cmd.none )


tick : Timer -> Timer
tick timer =
    { timer | milliSecLeft = timer.milliSecLeft - 1000 }


setWorkState : Timer -> Timer
setWorkState timer =
    let
        updatedCompletedPomodoros =
            if timer.completedPomodoros == 4 then
                0

            else
                timer.completedPomodoros
    in
    { timer
        | state = Work
        , completedPomodoros = updatedCompletedPomodoros
        , completedSets = timer.completedSets
        , milliSecLeft = 1000
        , paused = True
    }


setBreakState : Timer -> Timer
setBreakState timer =
    { timer
        | state = Break
        , completedPomodoros = timer.completedPomodoros + 1
        , completedSets = timer.completedSets
        , milliSecLeft = 1000
        , paused = True
    }


setLongBreakState : Timer -> Timer
setLongBreakState timer =
    { timer
        | state = LongBreak
        , completedPomodoros = timer.completedPomodoros + 1
        , completedSets = timer.completedSets + 1
        , milliSecLeft = 1000
        , paused = True
    }



-- TODO: find a proper ID


addTask : String -> List Task -> List Task
addTask text tasks =
    tasks ++ [ { id = String.length text, name = text, completed = False } ]


removeTask : Int -> List Task -> List Task
removeTask index list =
    List.take index list ++ List.drop (index + 1) list


toggleAtIndex : Int -> List Task -> List Task
toggleAtIndex indexToToggle list =
    List.indexedMap
        (\currentIndex todo ->
            if currentIndex == indexToToggle then
                { todo | completed = not todo.completed }

            else
                todo
        )
        list



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


stateToString : PomodoroState -> String
stateToString state =
    case state of
        Work ->
            "work"

        Break ->
            "break"

        LongBreak ->
            "longBreak"


checkmarkCircles : Timer -> Html Msg
checkmarkCircles timer =
    let
        emptyCircle =
            div
                [ css
                    [ padding (px 10)
                    , backgroundColor (hex "FFCFCF")
                    , borderStyle solid
                    , borderColor (hex "d93434")
                    , borderWidth (px 2)
                    , borderRadius (px 60)
                    , margin2 (em 0) (em 0.3)
                    ]
                ]
                []

        fullCircle =
            div
                [ css
                    [ padding (px 10)
                    , backgroundColor (hex "d93434")
                    , borderStyle solid
                    , borderColor (hex "d93434")
                    , borderWidth (px 2)
                    , borderRadius (px 60)
                    , margin2 (em 0) (em 0.3)
                    ]
                ]
                []

        allCircles =
            List.repeat timer.completedPomodoros fullCircle ++ List.repeat (4 - timer.completedPomodoros) emptyCircle
    in
    div
        [ css
            [ displayFlex
            , justifyContent center
            ]
        ]
        (List.map (\element -> element) allCircles)



-- todo: show full circles depending on data in timer state


timerView : Int -> Html Msg
timerView milliSeconds =
    let
        minute =
            let
                minutesInt =
                    Time.toMinute Time.utc (Time.millisToPosix milliSeconds)
            in
            if minutesInt < 10 then
                "0" ++ String.fromInt minutesInt

            else
                String.fromInt minutesInt

        seconds =
            let
                secondsInt =
                    Time.toSecond Time.utc (Time.millisToPosix milliSeconds)
            in
            if secondsInt < 10 then
                "0" ++ String.fromInt secondsInt

            else
                String.fromInt secondsInt
    in
    div
        [ css
            [ displayFlex
            , justifyContent center
            , fontFamilies
                [ "Lucida Sans Unicode", "sans-serif" ]
            ]
        ]
        [ text (minute ++ ":" ++ seconds) ]


controlsView : Timer -> Html Msg
controlsView timer =
    div
        [ css
            [ displayFlex
            , justifyContent center
            , alignItems center
            ]
        ]
        [ button
            [ css
                [ displayFlex
                , justifyContent center
                , alignItems center
                , margin (px 10)
                , padding (px 10)
                , backgroundColor (hex "d93434")
                , color (hex "fff")
                , borderRadius (px 60)
                , borderStyle none
                , pseudoClass "hover"
                    [ cursor pointer
                    ]
                ]
            , onClick TogglePauseTimer
            ]
            [ if timer.paused then
                Html.Styled.fromUnstyled (FeatherIcons.playCircle |> FeatherIcons.toHtml [])

              else
                Html.Styled.fromUnstyled (FeatherIcons.pauseCircle |> FeatherIcons.toHtml [])
            ]
        ]


taskListView : List Task -> Html Msg
taskListView list =
    ul
        [ css
            [ displayFlex
            , justifyContent center
            , alignItems center
            , flexDirection column
            , paddingLeft (px 0)
            ]
        ]
        (List.indexedMap (\idx taskName -> taskView idx taskName) list)


taskView : Int -> Task -> Html Msg
taskView index task =
    li
        [ css
            [ fontFamilies [ "Lucida Sans Unicode", "sans-serif" ]
            ]
        ]
        [ text task.name ]


debugStateParams : Timer -> Html Msg
debugStateParams timer =
    div []
        [ div
            [ css
                [ fontFamilies [ "Courier New", "monospace" ]
                ]
            ]
            [ text ("completedPomodoros" ++ " " ++ String.fromInt timer.completedPomodoros) ]
        , div
            [ css
                [ fontFamilies [ "Courier New", "monospace" ]
                ]
            ]
            [ text ("completedSets" ++ " " ++ String.fromInt timer.completedSets) ]
        , div
            [ css
                [ fontFamilies [ "Courier New", "monospace" ]
                ]
            ]
            [ text ("state" ++ " " ++ stateToString timer.state) ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        body =
            section
                [ css
                    [ displayFlex
                    , justifyContent center
                    , alignItems center
                    , marginTop (vh 10)
                    , fontFamilies [ "Palatino Linotype", "Georgia", "serif" ]
                    ]
                ]
                [ div []
                    [ h1
                        [ css
                            [ fontFamilies [ "Courier New", "monospace" ]
                            ]
                        ]
                        [ text "Pomodoro Timer" ]

                    -- , debugStateParams model.timer
                    , timerView model.timer.milliSecLeft
                    , span [] [ text (stateToString model.timer.state) ]
                    , controlsView model.timer
                    , checkmarkCircles model.timer
                    , div
                        [ css
                            [ displayFlex
                            , justifyContent center
                            , alignItems center
                            ]
                        ]
                        [ input [ value model.inputText, onInput ChangeInput, placeholder "Add Task" ] []
                        , button
                            [ css
                                [ displayFlex
                                , justifyContent center
                                , alignItems center
                                , margin (px 10)
                                , padding2 (px 5) (px 10)
                                ]
                            , onClick AddTask
                            ]
                            [ text "Add" ]
                        ]
                    , taskListView model.tasks
                    ]
                ]
    in
    { body = [ Html.Styled.toUnstyled body ]
    , title = "🍅 Pomodoro Timer"
    }
