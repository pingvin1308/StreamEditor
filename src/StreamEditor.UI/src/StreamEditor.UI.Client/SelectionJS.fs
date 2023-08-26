module StreamEditor.UI.Client.SelectionJS

open Microsoft.JSInterop

type Direction = 
    | Forward
    | Backward

let getDirectionValue direction =
    match direction with
    | Forward -> "right"
    | Backward -> "left"

let getCurrentPosition (js: IJSRuntime) =
    task {
        let! currentPosition = js.InvokeAsync<int>("SelectionJS.getCurrentPosition")
        return currentPosition
    }

let setOffset (js: IJSRuntime) (offset: int) =
    task { do! js.InvokeVoidAsync("SelectionJS.setOffset", offset) }

let moveCharacter (js: IJSRuntime) (direction: Direction) =
    task { do! js.InvokeVoidAsync("SelectionJS.moveCharacter", (getDirectionValue direction)) }

let moveLine (js: IJSRuntime) (direction: Direction) =
    task { do! js.InvokeVoidAsync("SelectionJS.moveLine", (getDirectionValue direction)) }
