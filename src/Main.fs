module Main

open Feliz
open Elmish
open Elmish.React

type Workout = {
    Name: string
    Reps: string
}

type Model = {
    CurrentWorkout: Workout option
}

type Msg =
    | GenerateWorkout

let allWorkouts = [
    { Name = "Push-ups"; Reps = "3 x 15" }
    { Name = "Squats"; Reps = "4 x 20" }
    { Name = "Burpees"; Reps = "3 x 10" }
    { Name = "Plank"; Reps = "2 x 1 min" }
    { Name = "Jumping Jacks"; Reps = "3 x 30" }
    { Name = "Lunges"; Reps = "3 x 12 per leg" }
    { Name = "Mountain Climbers"; Reps = "3 x 40" }
    { Name = "High Knees"; Reps = "3 x 30 sec" }
]

let init () = { CurrentWorkout = None }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | GenerateWorkout ->
        let rnd = System.Random()
        let workout = allWorkouts.[rnd.Next(allWorkouts.Length)]
        { model with CurrentWorkout = Some workout }, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.style [
            style.display.flex
            style.flexDirection.column
            style.alignItems.center
            style.justifyContent.center
            style.height (length.vh 100)
            style.fontFamily "Segoe UI"
        ]
        prop.children [

            Html.h1 [
                prop.text "Random Workout Generator"
                prop.style [ style.marginBottom 20 ]
            ]

            Html.button [
                prop.text "Generate Workout"
                prop.style [
                    style.padding (10, 20)
                    style.fontSize 18
                    style.cursor.pointer
                    style.marginBottom 30
                ]
                prop.onClick (fun _ -> dispatch GenerateWorkout)
            ]

            match model.CurrentWorkout with
            | Some workout ->
                Html.div [
                    Html.h2 workout.Name
                    Html.p workout.Reps
                ]
            | None ->
                Html.p "Click the button to get your workout!"
        ]
    ]

Program.mkProgram init update view
|> Program.withReactBatched "root"
|> Program.run
