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

type ExercisePhase = 
    | WorkPhase of int // seconds for work
    | RestPhase of int // seconds for rest

type Model = {
    WorkoutPlan: Workout list
    IsIntense: bool
    GuidedActive: bool
    CurrentExerciseIndex: int
    CurrentPhaseIndex: int
    CurrentSet: int
    TotalSets: int
    Phases: ExercisePhase list
    TimerCurrent: int
    TimerActive: bool
    IsInTransition: bool
}

type Msg =
    | GenerateWorkoutPlan
    | ToggleIntensity of bool
    | StartGuidedWorkout
    | StopGuidedWorkout
    | Tick
    | SkipSet

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
    { Name = "Child's Pose"; RepsNormal = "2 x 30 sec"; RepsIntense = "3 x 45 sec" }
    { Name = "Cat-Cow Stretch"; RepsNormal = "3 x 20 sec"; RepsIntense = "4 x 30 sec" }
    { Name = "Cobra Stretch"; RepsNormal = "2 x 30 sec"; RepsIntense = "3 x 45 sec" }
]

let init () = 
    { WorkoutPlan = []
      IsIntense = false
      GuidedActive = false
      CurrentExerciseIndex = 0
      CurrentPhaseIndex = 0
      CurrentSet = 1
      TotalSets = 0
      Phases = []
      TimerCurrent = 0
      TimerActive = false
      IsInTransition = false }, Cmd.none

let tickCmd =
    Cmd.OfAsync.perform (fun () -> async {
        do! Async.Sleep 1000
        return ()
    }) () (fun () -> Tick)

let parseReps (reps: string) =
    if reps.Contains "x" then
        let parts = reps.Split([|'x'|], System.StringSplitOptions.RemoveEmptyEntries)
        if parts.Length = 2 then
            let sets = parts.[0].Trim() |> int
            let repPart = parts.[1].Trim()
            
            if repPart.Contains "sec" then
                let seconds = repPart.Replace("sec", "").Trim() |> int
                [for _ in 1..sets -> [WorkPhase seconds; RestPhase 15]] |> List.concat
            elif repPart.Contains "min" then
                let minutes = repPart.Replace("min", "").Trim() |> int
                [for _ in 1..sets -> [WorkPhase (minutes * 60); RestPhase 15]] |> List.concat
            else
                let repCount = repPart.Replace("per leg", "").Trim() |> int
                [for _ in 1..sets -> [WorkPhase (repCount * 2); RestPhase 15]] |> List.concat
        else
            [WorkPhase 30]
    else
        [WorkPhase 30]

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
        if model.WorkoutPlan.IsEmpty then model, Cmd.none
        else
            let currentWorkout = model.WorkoutPlan.[0]
            let reps = if model.IsIntense then currentWorkout.RepsIntense else currentWorkout.RepsNormal
            let phases = parseReps reps
            { model with 
                GuidedActive = true 
                CurrentExerciseIndex = 0
                CurrentPhaseIndex = 0
                CurrentSet = 1
                TotalSets = phases |> List.filter (function WorkPhase _ -> true | _ -> false) |> List.length
                Phases = phases
                TimerCurrent = match phases.[0] with WorkPhase s -> s | RestPhase s -> s
                TimerActive = true
                IsInTransition = false }, tickCmd

    | StopGuidedWorkout ->
        { model with GuidedActive = false; CurrentExerciseIndex = 0; TimerActive = false; IsInTransition = false }, Cmd.none

    | SkipSet ->
        if model.GuidedActive then
            let rec findNextWorkPhase idx =
                if idx >= model.Phases.Length then None
                else
                    match model.Phases.[idx] with
                    | WorkPhase _ -> Some idx
                    | _ -> findNextWorkPhase (idx + 1)
            
            match findNextWorkPhase (model.CurrentPhaseIndex + 1) with
            | Some nextWorkIdx ->
                { model with
                    CurrentPhaseIndex = nextWorkIdx
                    TimerCurrent = match model.Phases.[nextWorkIdx] with WorkPhase s -> s | RestPhase s -> s
                    CurrentSet = model.CurrentSet + 1 }, 
                tickCmd
            | None ->
                let nextExerciseIndex = model.CurrentExerciseIndex + 1
                if nextExerciseIndex < model.WorkoutPlan.Length then
                    let nextWorkout = model.WorkoutPlan.[nextExerciseIndex]
                    let reps = if model.IsIntense then nextWorkout.RepsIntense else nextWorkout.RepsNormal
                    let phases = parseReps reps
                    { model with 
                        CurrentExerciseIndex = nextExerciseIndex
                        CurrentPhaseIndex = 0
                        CurrentSet = 1
                        TotalSets = phases |> List.filter (function WorkPhase _ -> true | _ -> false) |> List.length
                        Phases = phases
                        TimerCurrent = match phases.[0] with WorkPhase s -> s | RestPhase s -> s
                        IsInTransition = false }, 
                    tickCmd
                else
                    { model with GuidedActive = false; TimerActive = false }, Cmd.none
        else
            model, Cmd.none

    | Tick ->
        if not model.TimerActive then model, Cmd.none
        elif model.TimerCurrent > 1 then
            { model with TimerCurrent = model.TimerCurrent - 1 }, tickCmd
        else
            if model.CurrentPhaseIndex < model.Phases.Length - 1 then
                let nextPhaseIndex = model.CurrentPhaseIndex + 1
                let nextPhase = model.Phases.[nextPhaseIndex]
                let nextTimer = match nextPhase with WorkPhase s -> s | RestPhase s -> s
                let isRestPhase = match nextPhase with RestPhase _ -> true | _ -> false
                let nextSet = 
                    if not isRestPhase && nextPhaseIndex > 0 then 
                        match model.Phases.[nextPhaseIndex-1] with 
                        | RestPhase _ -> model.CurrentSet + 1 
                        | _ -> model.CurrentSet
                    else model.CurrentSet
                
                { model with 
                    CurrentPhaseIndex = nextPhaseIndex
                    TimerCurrent = nextTimer
                    IsInTransition = isRestPhase
                    CurrentSet = nextSet }, tickCmd
            else
                let nextExerciseIndex = model.CurrentExerciseIndex + 1
                if nextExerciseIndex < model.WorkoutPlan.Length then
                    let nextWorkout = model.WorkoutPlan.[nextExerciseIndex]
                    let reps = if model.IsIntense then nextWorkout.RepsIntense else nextWorkout.RepsNormal
                    let phases = parseReps reps
                    { model with 
                        CurrentExerciseIndex = nextExerciseIndex
                        CurrentPhaseIndex = 0
                        CurrentSet = 1
                        TotalSets = phases |> List.filter (function WorkPhase _ -> true | _ -> false) |> List.length
                        Phases = phases
                        TimerCurrent = match phases.[0] with WorkPhase s -> s | RestPhase s -> s
                        IsInTransition = false }, tickCmd
                else
                    { model with GuidedActive = false; TimerActive = false }, Cmd.none

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
                Html.div [
                    prop.style [ style.display.flex; style.marginTop 10 ]
                    prop.children [
                        Html.button [
                            prop.text "Stop Guided Workout"
                            prop.onClick (fun _ -> dispatch StopGuidedWorkout)
                        ]
                        Html.button [
                            prop.text "Skip Set"
                            prop.style [ style.marginLeft 10 ]
                            prop.onClick (fun _ -> dispatch SkipSet)
                        ]
                    ]
                ]

            if model.WorkoutPlan.IsEmpty then
                Html.p "Click the button to generate your workout plan!"
            else
                Html.div [
                    for i, workout in List.indexed model.WorkoutPlan ->
                        Html.div [
                            prop.style [ 
                                style.marginTop 10
                                style.marginBottom 10
                                style.borderWidth 2
                                style.borderStyle (if model.GuidedActive && i = model.CurrentExerciseIndex then borderStyle.solid else borderStyle.none)
                                style.borderColor (if model.GuidedActive && i = model.CurrentExerciseIndex then "green" else "transparent")
                                style.padding 10
                                style.borderRadius 5
                            ]
                            prop.children [
                                Html.h2 workout.Name
                                Html.p (if model.IsIntense then workout.RepsIntense else workout.RepsNormal)

                                if model.GuidedActive && i = model.CurrentExerciseIndex then
                                    let currentPhase = model.Phases.[model.CurrentPhaseIndex]
                                    let phaseName = match currentPhase with WorkPhase _ -> "WORK" | RestPhase _ -> "REST"
                                    let phaseDuration = match currentPhase with WorkPhase s -> s | RestPhase s -> s
                                    let percent = 100.0 - (float model.TimerCurrent / float phaseDuration * 100.0)
                                    
                                    Html.div [
                                        prop.style [ style.marginTop 10 ]
                                        prop.children [
                                            Html.h3 (sprintf "%s - Set %d/%d" phaseName model.CurrentSet model.TotalSets)
                                            Html.p (sprintf "Time remaining: %02d:%02d" (model.TimerCurrent / 60) (model.TimerCurrent % 60))
                                            
                                            Html.div [
                                                prop.style [
                                                    style.width (length.percent 100)
                                                    style.height 20
                                                    style.backgroundColor.lightGray
                                                    style.borderRadius 10
                                                    style.marginTop 10
                                                ]
                                                prop.children [
                                                    Html.div [
                                                        prop.style [
                                                            style.height 20
                                                            style.width (length.percent (int percent))
                                                            style.backgroundColor (match currentPhase with WorkPhase _ -> "green" | RestPhase _ -> "#FFA500")
                                                            style.borderRadius 10
                                                            style.transitionDuration (System.TimeSpan.FromSeconds 0.5)
                                                            style.transitionTimingFunction.ease
                                                            style.transitionProperty "width"
                                                        ]
                                                    ]
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