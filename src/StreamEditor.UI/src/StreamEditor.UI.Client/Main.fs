module StreamEditor.UI.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.JSInterop

/// Routing endpoints definition.
type Page = | [<EndPoint "/">] Editor

/// The Elmish application's model.
type Model =
    { page: Page
      error: string option
      code: string
      isTyping: bool
      formattedCode: Node[]
      textWidthToCursor: float
      topCursorPosition: int
      leftCursorPosition: int
      topSelectPosition: int
      leftSelectPosition: int }

let initModel =
    { page = Editor
      error = None
      code = ""
      isTyping = false
      formattedCode = [||]
      textWidthToCursor = 0
      topCursorPosition = 0
      leftCursorPosition = 0
      topSelectPosition = 0
      leftSelectPosition = 0 }

type ArrowKey =
    | ArrowDown
    | ArrowUp
    | ArrowLeft
    | ArrowRight

type SpecialKey =
    | Arrows of ArrowKey
    | Enter
    | Backspace
    | Symbol

let parseArrowKey =
    function
    | "ArrowDown" -> Some ArrowDown
    | "ArrowUp" -> Some ArrowUp
    | "ArrowLeft" -> Some ArrowLeft
    | "ArrowRight" -> Some ArrowRight
    | _ -> None

let parseSpecialKey key =
    let specialKey =
        match key with
        | "Enter" -> Some Enter
        | "Backspace" -> Some Backspace
        | "Alt"
        | "Control"
        | "Shift" -> None
        | _ ->
            match parseArrowKey key with
            | Some arrows -> Some(Arrows arrows)
            | None -> Some Symbol

    specialKey

type Message =
    | SetPage of Page
    | OnCodeEdit of string
    | OnCaretMove of ArrowKey
    | OnSelect of ArrowKey
    | OnRemove
    | OnInsert of string
    | OnSetCursor of int * int
    | Error of exn
    | ClearError

let calcCursorIndex model =
    if model.code.Length = 0 then
        0
    else
        let lines = model.code.Split "\n"

        let previousLines =
            if lines.Length > 0 then
                lines[.. (model.topCursorPosition - 1)]
            else
                [||]

        let symbolsInPreviousLines =
            previousLines |> Array.map (fun x -> x.Length) |> Array.sum

        let index =
            symbolsInPreviousLines + model.topCursorPosition + model.leftCursorPosition

        index

let update (js: IJSRuntime) message model =

    match message with
    | SetPage page -> { model with page = page }, Cmd.none
    | OnCodeEdit code ->
        let lines = code.Split "\n"

        let nodes =
            lines
            |> Array.mapi (fun i line ->
                line.Split " "
                |> Array.map (fun word ->
                    span {
                        on.mouseup (fun (args) ->
                            printfn "start select"
                            ())

                        word
                        " "
                    }))
            |> Array.mapi (fun i line ->
                span {
                    for word in line do
                        word

                    "\n"
                })

        let isAppend = model.code.Length < code.Length

        let maxColumnPosition topCursorPosition = lines[topCursorPosition].Length

        let (top, left, cmd) =
            match (isAppend, lines.Length) with
            | (isAppend, linesCount) when isAppend && linesCount > (model.code.Split "\n").Length ->
                let top = model.topCursorPosition + 1
                let moveDown = (Cmd.OfTask.attempt (SelectionJS.moveLine js) SelectionJS.Forward)
                top, 0, moveDown

            | (isAppend, linesCount) when (not isAppend) && linesCount < (model.code.Split "\n").Length ->
                let top = model.topCursorPosition - 1
                let moveUp = (Cmd.OfTask.attempt (SelectionJS.moveLine js) SelectionJS.Backward)
                top, maxColumnPosition top, moveUp

            | (isAppend, linesCount) when isAppend && linesCount = (model.code.Split "\n").Length ->
                let moveRight = (Cmd.OfTask.attempt (SelectionJS.moveCharacter js) SelectionJS.Forward)
                model.topCursorPosition, model.leftCursorPosition + 1, moveRight

            | (isAppend, linesCount) when not isAppend && linesCount = (model.code.Split "\n").Length ->
                let moveLeft = (Cmd.OfTask.attempt (SelectionJS.moveCharacter js) SelectionJS.Backward)
                model.topCursorPosition, model.leftCursorPosition - 1, moveLeft

            | _ -> failwith "todo"

        { model with
            topCursorPosition = top
            leftCursorPosition = left
            formattedCode = nodes
            code = code },
        cmd Error

    | OnCaretMove arrowKey ->
        if model.code.Length > 0 then
            let lines = model.code.Split "\n"
            let maxLinePosition = lines.Length - 1
            let maxColumnPosition topCursorPosition = lines[topCursorPosition].Length

            let top, left =
                match arrowKey with
                | ArrowDown ->
                    let top = Math.Min(model.topCursorPosition + 1, maxLinePosition)
                    let left = Math.Min(model.leftCursorPosition, maxColumnPosition top)
                    top, left
                | ArrowUp ->
                    let top = Math.Max(model.topCursorPosition - 1, 0)
                    let left = Math.Min(model.leftCursorPosition, maxColumnPosition top)
                    top, left
                | ArrowLeft ->
                    let top = model.topCursorPosition
                    let left = Math.Max(model.leftCursorPosition - 1, 0)
                    top, left
                | ArrowRight ->
                    let top = model.topCursorPosition

                    let left =
                        Math.Min(model.leftCursorPosition + 1, maxColumnPosition model.topCursorPosition)

                    top, left

            { model with
                topCursorPosition = top
                leftCursorPosition = left },
            Cmd.none
        else
            model, Cmd.none

    | OnSelect arrowKey ->
        // start selection of characters from current position

        { model with
            topSelectPosition = 1
            leftSelectPosition = 4 },
        Cmd.ofMsg (OnCaretMove arrowKey)
    | OnInsert symbol ->
        let code =
            if symbol <> "" then
                let index = calcCursorIndex model
                model.code.Insert(index, symbol)
            else
                model.code

        model, Cmd.ofMsg (OnCodeEdit code)

    | OnSetCursor (top, left) ->

        { model with
            topCursorPosition = top
            leftCursorPosition = left },
        Cmd.none

    | OnRemove ->
        if model.code.Length > 0 then
            let index = calcCursorIndex model

            let code =
                if model.code.Length - 1 > index then
                    model.code.Remove(index - 1, 1)
                else
                    model.code.Remove(model.code.Length - 1)

            model, Cmd.ofMsg (OnCodeEdit code)
        else
            model, Cmd.none

    | Error exn -> { model with error = Some exn.Message }, Cmd.none
    | ClearError -> { model with error = None }, Cmd.none

let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let mutable blink = true

let viewEditor (js: IJSRuntime) model dispatch =
    div {
        attr.id "container"
        attr.tabindex "0"

        on.async.keydown (fun args ->
            async {
                // blink <- false

                match (parseSpecialKey args.Key) with
                | Some specialKey ->
                    match specialKey with
                    | Arrows arrows when args.ShiftKey -> dispatch (OnSelect arrows)
                    | Arrows arrows -> dispatch (OnCaretMove arrows)
                    | Enter -> dispatch (OnInsert "\n")
                    | Backspace -> dispatch OnRemove
                    | Symbol -> dispatch (OnInsert args.Key)
                | None -> ()

            // do! Task.Delay 1000 |> Async.AwaitTask
            // blink <- true
            })

        // on.async.keyup (fun _ ->
        //     async {
        //         do! Task.Delay 1000 |> Async.AwaitTask
        //         blink <- true
        //     })

        div {
            attr.id "display"
            attr.style "white-space: pre; display:flex;"

            div {
                attr.style
                    "white-space: pre;
                    display:flex;
                    flex-direction: column;
                    text-align:end;
                    width: 50px;
                    border-right: #363636;
                    border-style: none solid none none;
                    margin-right: 5px;"

                let getLineNumberElement num = span { string (num + 1) + ": " }

                let lineNumbersElements =
                    model.formattedCode |> Array.mapi (fun i _ -> getLineNumberElement i)

                if lineNumbersElements.Length = 0 then
                    getLineNumberElement 0
                else
                    for lineElement in lineNumbersElements do
                        lineElement
            }

            div {
                attr.style "position:relative"


                let lines =
                    model.formattedCode
                    |> Array.mapi (fun i line ->
                        pre {
                            on.task.click (fun (args) ->
                                task {
                                    let! leftPosition = SelectionJS.getCurrentPosition js
                                    dispatch (OnSetCursor(i, leftPosition))
                                })

                            line
                        })

                for line in lines do
                    line

                div {
                    attr.id "cursor"

                    attr.style (
                        sprintf
                            "top:%dem; left:%dch; animation:%s;"
                            (model.topCursorPosition)
                            (model.leftCursorPosition)
                            (if blink then
                                 "cursor-blink 1.5s steps(2) infinite"
                             else
                                 "pause")
                    )
                }
            }
        }
    }

let viewStreamEditorPage (js: IJSRuntime) model dispatch =
    div {
        h1 { "Stream editor" }
        span { "top:" + (string model.topCursorPosition) }
        span { "left:" + (string model.leftCursorPosition) }
        hr
        viewEditor js model dispatch
    }

let view (js: IJSRuntime) model dispatch =
    Main()
        .Body(
            cond model.page
            <| function
                | Editor -> viewStreamEditorPage js model dispatch
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let update = update this.JSRuntime
        let view = view this.JSRuntime

        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
