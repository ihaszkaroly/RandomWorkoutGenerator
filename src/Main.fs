module Main

open Feliz
open Elmish
open Elmish.React

type Workout = {
    Name: string
    RepsNormal: string
    RepsIntense: string
}

type Model = {
    WorkoutPlan: Workout list
    IsIntense: bool
}

type Msg =
    | GenerateWorkoutPlan
    | ToggleIntensity of bool

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

let init () = { WorkoutPlan = []; IsIntense = false }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | GenerateWorkoutPlan ->
        let rnd = System.Random()
        let plan =
            allWorkouts
            |> List.sortBy (fun _ -> rnd.Next()) // Shuffle
            |> List.take 3
        { model with WorkoutPlan = plan }, Cmd.none
    | ToggleIntensity state ->
        { model with IsIntense = state }, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
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

            Html.h1 [
                prop.text "Random Workout Plan Generator"
                prop.style [ style.marginBottom 10 ]
            ]

            Html.div [
                prop.style [ style.marginBottom 20 ]
                prop.children [
                    Html.label [
                        Html.input [
                            prop.type'.checkbox
                            prop.isChecked model.IsIntense
                            prop.onChange (fun ev -> dispatch (ToggleIntensity ev))
                        ]
                        Html.span [
                            prop.text " Intense Mode"
                            prop.style [ style.marginLeft 10 ]
                        ]
                    ]
                ]
            ]

            Html.button [
                prop.text "Generate Workout Plan"
                prop.style [
                    style.padding (10, 20)
                    style.fontSize 18
                    style.cursor.pointer
                    style.marginBottom 30
                ]
                prop.onClick (fun _ -> dispatch GenerateWorkoutPlan)
            ]

            if model.WorkoutPlan.IsEmpty then
                Html.p "Click the button to generate your workout plan!"
            else
                Html.div [
                    for workout in model.WorkoutPlan ->
                        Html.div [
                            Html.h2 workout.Name
                            Html.p (if model.IsIntense then workout.RepsIntense else workout.RepsNormal)
                        ]
                ]
        ]
    ]

Program.mkProgram init update view
|> Program.withReactBatched "root"
|> Program.run
