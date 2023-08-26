module StreamEditor.UI.Client.FocusJS

open Microsoft.JSInterop

let getWidth (js: IJSRuntime) (text: string) =
    task {
        let! width = js.InvokeAsync<int>("FocusJS.getTextWidth", text)
        return width
    }