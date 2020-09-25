module Main exposing (main)

import Browser
import Css exposing (..)
import Html.Styled exposing (Html, button, div, h1, input, li, section, text, ul)
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
    , milliSecLeft = 1500000
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { timer = defaultPomodoroState, inputText = "", tasks = defaultTasks }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AddTask
    | RemoveTask Int
    | ToggleTask Int
    | ChangeInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick _ ->
            ( { model | timer = tick model.timer }, Cmd.none )

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


timerView : Int -> Html Msg
timerView milliSeconds =
    let
        minute =
            String.fromInt (Time.toMinute Time.utc (Time.millisToPosix milliSeconds))

        second =
            String.fromInt (Time.toSecond Time.utc (Time.millisToPosix milliSeconds))
    in
    div
        [ css
            [ fontFamilies [ "Lucida Sans Unicode", "sans-serif" ]
            ]
        ]
        [ text (minute ++ ":" ++ second) ]


taskListView : List Task -> Html Msg
taskListView list =
    ul
        [ css
            [ displayFlex
            , justifyContent center
            , alignItems center
            , flexDirection column
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
                [ timerView model.timer.milliSecLeft
                , div []
                    [ h1
                        [ css
                            [ fontFamilies [ "Courier New", "monospace" ]
                            ]
                        ]
                        [ text "Pomodoro Timer" ]
                    , div
                        [ css
                            [ displayFlex
                            , justifyContent center
                            , alignItems center
                            ]
                        ]
                        [ input [ value model.inputText, onInput ChangeInput, placeholder "placeholder" ] []
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
