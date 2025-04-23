module Main

open Fable.Core
open Feliz
open Elmish
open Elmish.React

let [<Global>] setInterval (_: unit -> unit) (_: int) = 0
let [<Global>] clearInterval (_: int) = ()

type Workout = {
    Name: string
    RepsNormal: string
    RepsIntense: string
}

type Model = {
    WorkoutPlan: Workout list
    IsIntense: bool
    GuidedActive: bool
    CurrentExerciseIndex: int
    TimerCurrent: int
    TimerMax: int
    TimerActive: bool
    TimerBetweenExercises: int
    IsInTransition: bool
}

type Msg =
    | GenerateWorkoutPlan
    | ToggleIntensity of bool
    | StartGuidedWorkout
    | StopGuidedWorkout
    | Tick

let allWorkouts = [
    { Name = "Push-ups"; RepsNormal = "3 x 15"; RepsIntense = "5 x 20" }
    { Name = "Tricep Dips"; RepsNormal = "3 x 12"; RepsIntense = "5 x 15" }
    { Name = "Shoulder Taps"; RepsNormal = "3 x 20"; RepsIntense = "4 x 30" }
    { Name = "Arm Circles"; RepsNormal = "3 x 30 sec"; RepsIntense = "4 x 45 sec" }
    { Name = "Squats"; RepsNormal = "4 x 20"; RepsIntense = "6 x 25" }
    { Name = "Lunges"; RepsNormal = "3 x 12 per leg"; RepsIntense = "5 x 15 per leg" }
    { Name = "Wall Sit"; RepsNormal = "2 x 45 sec"; RepsIntense = "3 x 1 min" }
    { Name = "Calf Raises"; RepsNormal = "3 x 25"; RepsIntense = "5 x 30" }
    { Name = "Plank"; RepsNormal = "3 x 1 min"; RepsIntense = "4 x 90 sec" }
    { Name = "Sit-ups"; RepsNormal = "3 x 20"; RepsIntense = "5 x 30" }
    { Name = "Leg Raises"; RepsNormal = "3 x 15"; RepsIntense = "4 x 20" }
    { Name = "Russian Twists"; RepsNormal = "3 x 40"; RepsIntense = "5 x 60" }
    { Name = "Burpees"; RepsNormal = "3 x 10"; RepsIntense = "5 x 15" }
    { Name = "Jumping Jacks"; RepsNormal = "3 x 30"; RepsIntense = "5 x 50" }
    { Name = "Mountain Climbers"; RepsNormal = "3 x 40"; RepsIntense = "5 x 60" }
    { Name = "High Knees"; RepsNormal = "3 x 30 sec"; RepsIntense = "5 x 45 sec" }
    { Name = "Forward Fold Stretch"; RepsNormal = "2 x 30 sec"; RepsIntense = "3 x 45 sec" }
    { Name = "Child’s Pose"; RepsNormal = "2 x 30 sec"; RepsIntense = "3 x 45 sec" }
    { Name = "Cat-Cow Stretch"; RepsNormal = "3 x 20 sec"; RepsIntense = "4 x 30 sec" }
    { Name = "Cobra Stretch"; RepsNormal = "2 x 30 sec"; RepsIntense = "3 x 45 sec" }
]

let init () = 
    { WorkoutPlan = []
      IsIntense = false
      GuidedActive = false
      CurrentExerciseIndex = 0
      TimerCurrent = 0
      TimerMax = 30
      TimerActive = false
      TimerBetweenExercises = 5
      IsInTransition = false }, Cmd.none

let tickCmd =
    Cmd.OfAsync.perform (fun () -> async {
        do! Async.Sleep 1000
        return ()
    }) () (fun () -> Tick)


let update msg model =
    match msg with
    | GenerateWorkoutPlan ->
        let rnd = System.Random()
        let plan =
            allWorkouts
            |> List.sortBy (fun _ -> rnd.Next())
            |> List.take 3
        { model with WorkoutPlan = plan }, Cmd.none

    | ToggleIntensity state ->
        { model with IsIntense = state }, Cmd.none

    | StartGuidedWorkout ->
        { model with GuidedActive = true; CurrentExerciseIndex = 0; TimerCurrent = model.TimerMax; TimerActive = true; IsInTransition = false }, tickCmd

    | StopGuidedWorkout ->
        { model with GuidedActive = false; CurrentExerciseIndex = 0; TimerActive = false; IsInTransition = false }, Cmd.none

    | Tick ->
        if not model.TimerActive then model, Cmd.none
        elif model.TimerCurrent > 0 then
            { model with TimerCurrent = model.TimerCurrent - 1 }, tickCmd
        else
            if model.IsInTransition then
                let nextIndex = model.CurrentExerciseIndex + 1
                if nextIndex < model.WorkoutPlan.Length then
                    { model with CurrentExerciseIndex = nextIndex; TimerCurrent = model.TimerMax; IsInTransition = false }, tickCmd
                else
                    { model with GuidedActive = false; TimerActive = false; IsInTransition = false }, Cmd.none
            else
                { model with TimerCurrent = model.TimerBetweenExercises; IsInTransition = true }, tickCmd

let view model dispatch =
    Html.div [
        prop.style [
            style.display.flex
            style.flexDirection.column
            style.alignItems.center
            style.justifyContent.center
            style.height (length.vh 100)
            style.fontFamily "Segoe UI"
            style.padding 20
        ]
        prop.children [
            Html.h1 "Random Workout Plan Generator"

            Html.div [
                Html.label [
                    Html.input [
                        prop.type'.checkbox
                        prop.isChecked model.IsIntense
                        prop.onChange (fun value -> dispatch (ToggleIntensity value))
                    ]
                    Html.span [ prop.text " Intense Mode" ]
                ]
            ]

            Html.button [
                prop.text "Generate Workout Plan"
                prop.onClick (fun _ -> dispatch GenerateWorkoutPlan)
            ]

            Html.button [
                prop.text "Start Guided Workout"
                prop.disabled (model.WorkoutPlan.IsEmpty || model.GuidedActive)
                prop.onClick (fun _ -> dispatch StartGuidedWorkout)
            ]

            if model.GuidedActive then
                Html.button [
                    prop.text "Stop Guided Workout"
                    prop.onClick (fun _ -> dispatch StopGuidedWorkout)
                ]

            if model.WorkoutPlan.IsEmpty then
                Html.p "Click the button to generate your workout plan!"
            else
                Html.div [
                    for i, workout in List.indexed model.WorkoutPlan ->
                        Html.div [
                            prop.style [ style.marginTop 10; style.marginBottom 10 ]
                            prop.children [
                                Html.h2 workout.Name
                                Html.p (if model.IsIntense then workout.RepsIntense else workout.RepsNormal)

                                if model.GuidedActive && i = model.CurrentExerciseIndex then
                                    let percent = 100.0 - (float model.TimerCurrent / float (if model.IsInTransition then model.TimerBetweenExercises else model.TimerMax) * 100.0)
                                    Html.div [
                                        prop.style [
                                            style.width (length.percent 100)
                                            style.height 20
                                            style.backgroundColor.lightGray
                                        ]
                                        prop.children [
                                            Html.div [
                                                prop.style [
                                                    style.height 20
                                                    style.width (length.percent (int percent))
                                                    style.backgroundColor (if model.IsInTransition then "#FFA500" else "green")
                                                ]
                                            ]
                                        ]
                                    ]
                            ]
                        ]
                ]
        ]
    ]

Program.mkProgram init update view
|> Program.withReactBatched "root"
|> Program.run