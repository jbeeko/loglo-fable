//TODO:
// *  row select
// *  row insert
// *  edit bar editor should not get blue focus
// *  edit bar should be reactive columns so the input stack is continguous
//    with the edit field and then contiguous with the output.
//  * Arrow down though readonly cells shits the sheet up


// ----------------------------------------------------------------------------
// LOGLO UI
// Loglo UI as demonstrated by: Avi Bryant's loglo.app
// In part derivative of: http://tomasp.net/blog/2018/write-your-own-excel/
namespace Loglo


module Application =
  open Elmish
  open Feliz

  open Domain

  type EditMode =
  | Initial
  | PartialFocus
  | FullFocus

  type EditState = {
    Pos: Position
    Cell: Cell
    Focus: EditMode
    Orig: Cell
  }

  type DisplayMode =
  | Inputs
  | Values

  type State = {
    Sheet: Sheet
    EditState: EditState option
    DisplayMode: DisplayMode
  }

  // ----------------------------------------------------------------------------
  // MESSAGE HANDLING
  // ----------------------------------------------------------------------------
  type Message =
    | StartEdit of Position
    | EndEdit of bool
    | ToggleDisplayMode
    | UpdateCell of Position * string
    | UpdateEditValue of Position * string

  let update msg state =
    match msg with
    | StartEdit pos ->
      if Sheet.contains pos state.Sheet then
        let cell = Sheet.find pos state.Sheet
        let state' =
          match state.EditState with
          | Some es when es.Pos = pos && es.Focus <> FullFocus ->
            { state with EditState = Some {es with Focus = FullFocus} }
          | Some es when es.Pos = pos && es.Focus = FullFocus -> state
          | _  ->
            { state with EditState = Some {Pos = pos; Cell = cell; Orig = cell; Focus = Initial} }
        state', Cmd.Empty
      else state, Cmd.Empty
    | EndEdit cancel ->
      match state.EditState, cancel with
      | Some es, true ->
        {state with Sheet = Sheet.upsert es.Pos state.Sheet es.Orig; EditState = None}, Cmd.Empty
      | _, _ -> state, Cmd.Empty
    | ToggleDisplayMode ->
      match state.DisplayMode with
      | Inputs -> {state with DisplayMode = Values }, Cmd.Empty
      | Values -> {state with DisplayMode = Inputs}, Cmd.Empty
    | UpdateCell (pos, value) ->
        let sheet = Evaluator.recalc pos state.Sheet false value
        {state with Sheet = sheet}, Cmd.Empty
    | UpdateEditValue (pos, value) ->
        let cell =
          Evaluator.recalc pos state.Sheet true value
          |> Sheet.find pos
        let editState =
          match state.EditState with
          | Some es when es.Focus = Initial -> Some {es with Focus = PartialFocus; Cell = {cell with Input = value}}
          | Some es -> Some {es with Cell = {cell with Input = value}}
          | None -> None
        {state with EditState = editState}, Cmd.Empty


  // ----------------------------------------------------------------------------
  // RENDERING
  // ----------------------------------------------------------------------------
  let printType v =
    match v with
    | None -> "?"
    | Some (v, cmnt) ->
      (match (v, cmnt) with
      | Int i, _ -> "num "+cmnt
      | Text s, _ -> "text "+cmnt
      | Name s, _ -> "name "+cmnt
      | Binding s, _ -> "def "+cmnt
      | Code l, _ -> "{} "+cmnt
      | Paths l, _ -> "Paths "+cmnt
      | Error (p,e), _ -> "Error "+cmnt
      | Nil, _ -> "nil "+cmnt).Trim()

  let printValue v =
    match v with
    | Int i -> sprintf "%i" i
    | Text s -> sprintf "\"%s\"" s
    | Name s -> sprintf ":%s" s
    | Binding s -> sprintf "%s" s
    | Code l -> "{}"
    | Paths l -> sprintf "%i Paths" l.Length
    | Error (p,e) -> "Err"
    | Nil -> "nil"

  let errorCodeAndComment (_, e) =
    match e with
    | StackUnderflow (expected, given) ->
        "Underflow", sprintf "needed %i params got %i" expected given
    | ParseFailure s ->
        "BadInput", sprintf "%s" s
    | ReferenceCycle (Position(c,r)::t) ->
        let cycle =
          List.fold (fun s (Position (c, r)) -> s + (sprintf "-%c%i" c r)) "" t
        "Loop", sprintf "%c%i%s" c r cycle
    | SelfReference ->
        "SelfRef", ""
    | DivideByZero i ->
        "DivByZero", sprintf "%i/0" i
    | MissingDefinition s ->
        "Undefined", sprintf "%s" s
    | DuplicateDefinition (s, v) ->
        "Duplicate", sprintf "%s is %s" s (printValue v)
    | ReferenceOffSheet (Position (c, r)) ->
        "OffSheet", sprintf "%c%i" c r
    | InvalidParams (op, expected, given) ->
        let expected = System.String.Join(", ", List.map printType expected)
        let given = System.String.Join(", ", List.map printValue given)
        "BadInput", sprintf "%s needed %s but got  %s" op expected given
    | MissingItem (Position (c, r)) ->
        "Empty", sprintf "%c%i" c r
    | NotImplemented s ->
        "Reserved", sprintf "%s" s
    | FilledCellOverwritten ->
        "ReadOnly", ""
    | _ -> "Err", e.ToString()

  let errorMsg (p, e) =
    match errorCodeAndComment (p, e) with
    | c, "" -> c
    | c, d -> sprintf "%s: %s" c d



  let renderCellEditor colSpan dispatch state =
    // State machine for editing as follows:
    // * on initial click before typeing Focus = Initial
    //    - no insertion point
    //    - pointer is arrow
    //    - field highlighted
    //    - all existing text is implicitly (or explicit?) selected so typeing deletes it all
    //    - after typing insertion point is shown
    //    - right/left arrow keys navigate out of cell
    //    - furhter input is gathered up
    // * on second click, before or after typing; Focus = FullFocus
    //    - insertion point shown at point of click
    //    - pointer is bar
    //    - right/left arrow keys navigate text
    // * on doubleClick
    //    - ?
    // NOTE: all the above is true for both the edit bar and the in cell editor

    let moveTo pos sheet =
      if Sheet.contains pos sheet
      then
        let c = Sheet.find pos sheet
        dispatch (StartEdit pos)

    match state.EditState with
    | Some {Pos = pos; Cell = cell; Focus = focus} ->
      Html.td [
        prop.className [Bulma.IsSize7]
        prop.style [style.padding 0]
        prop.colSpan colSpan
        prop.children [
          Html.input [
            // prop.readOnly (cell.Type = Child) // scrolls sheet!?
            prop.className [Bulma.IsSize7; Bulma.Input; if colSpan = 1 then Bulma.IsFocused]
            prop.style [
              style.borderRadius 0
              style.fontWeight 500
              match state.EditState with
                | Some es when es.Focus = Initial ->
                  (Interop.mkStyle "caret-color" "transparent")
                  style.cursor.defaultCursor
                | _ -> ()
            ]
            prop.autoFocus true
            prop.type'.text
            prop.value cell.Input
            prop.onKeyDown (fun e ->
              match e.key with
              | "`" when e.ctrlKey -> dispatch ToggleDisplayMode
              | "Escape" -> dispatch (EndEdit true)

              | "Enter" when e.shiftKey ->dispatch(StartEdit (Position.up pos))
              | "Enter" -> dispatch(StartEdit (Position.down pos))

              | "Tab" when e.shiftKey ->
                e.preventDefault()
                dispatch(StartEdit (Position.left pos))
              | "Tab" ->
                e.preventDefault()
                dispatch(StartEdit (Position.right pos))

              | "ArrowUp" -> dispatch(StartEdit (Position.up pos))
              | "ArrowDown" -> dispatch(StartEdit (Position.down pos))
              | "ArrowLeft" when focus <> FullFocus -> dispatch(StartEdit (Position.left pos))
              | "ArrowRight" when focus <> FullFocus -> dispatch(StartEdit (Position.right pos))
              // Initial key stroke clears existing contact (as in Excel and numbers)
              // Don't clear for function, option alt etc.
              | key when key.Length = 1 || key = "Backspace" ->
                match state.EditState with
                | Some es when es.Focus = Initial ->
                  dispatch (UpdateEditValue(pos, ""))
                  if key = "Backspace" then dispatch (UpdateCell(pos, "")) else ()
                | _ -> ()
              | _ -> ())

            prop.onFocus (fun e -> ())
              // TODO - decide if text should be selected on entry
              // TODO - focust does not does not fire when arrowed into.
              // if colSpan = 1 then
              //   (e.currentTarget :?> Browser.Types.HTMLInputElement).select())

            prop.onClick (fun e -> dispatch(StartEdit pos))
            prop.onTextChange (fun e ->
              dispatch (UpdateCell(pos, e))) // dispatch on value accepted
            prop.onInput (fun e ->            // dispatch on each keystroke
              let txt = (e.currentTarget :?> Browser.Types.HTMLInputElement).value
              dispatch(UpdateEditValue(pos, txt)))]]]
    | _ -> failwith "should not happen"

  let topLeft = Html.td [prop.style [style.width 45]]
  let renderEditBar dispatch state =
    match state.EditState with
    | Some {Pos = pos; Cell = cell} ->
      Html.tr [
        topLeft
        // Pre stack
        Html.td [
          let stack = Sheet.findStackLeft pos state.Sheet
          let txt = System.String.Join(", ", stack |> List.rev |> List.map printValue)
          prop.colSpan 1
          prop.text (sprintf "[%s" txt)
          prop.style [style.textAlign.right]]

        renderCellEditor (state.Sheet.Cols.Length - 7) dispatch state

        // Post stack
        Html.td [
          let stack =
            match cell.Input.Trim() with
            | "" -> Sheet.findStackLeft pos state.Sheet
            | _ -> cell.Stack
          let txts =
            stack |> List.rev
            |> List.map (fun v ->
              match v with
              | Error (p, e) when pos = p ->  sprintf "(%s)" (errorMsg (p, e))
              | _ -> printValue v)
          let alert = List.exists (fun v -> match v with | Error (p, _) when p = pos -> true | _ -> false) stack
          let txt = System.String.Join(", ", txts)
          prop.colSpan 6
          prop.text ("[" + txt)
          if alert then prop.style [style.color "Red"]]]
      | _ ->
        Html.tr [
          topLeft
          Html.td [prop.text "[ "; prop.colSpan 1; prop.style [style.textAlign.right]]
          Html.td [prop.colSpan (state.Sheet.Cols.Length - 7)]
          Html.td [prop.text "[ "; prop.colSpan 6]]

  let private colourStyles cell = [
      match Cell.value cell, cell.Type with
      | _, Child-> style.backgroundColor "AliceBlue"
      | Error _, _ ->
          style.color "Red"
          style.backgroundColor "LightYellow"
      | Nil, _ -> style.backgroundColor "White"
      | _, _-> style.backgroundColor "LightYellow"]

  let renderValue dispatch pos state =
    // TODO - should draw Paths as SVG on a canvas, others as str
    // TODO - split cell, with stack and value below, any SVG above
    //        This will give more context.
    let cell = (Sheet.find pos state.Sheet)
    Html.td [
      prop.onClick (fun _ -> dispatch(StartEdit pos))
      prop.style ([style.whitespace.nowrap; style.overflow.hidden; style.textOverflow.ellipsis]@(colourStyles cell))
      prop.text
        (match (Cell.value cell) with
        | Paths p -> printValue (Paths p)
        | Error (p, e) -> if p = pos then errorMsg (p, e) else "Err"
        | Nil -> ""
        | v -> printValue v)]

  let renderInputs dispatch pos state =
    // TODO - show input to left and stack in gray to right
    let cell = (Sheet.find pos state.Sheet)
    let stack =
      match cell.Input.Trim() with
      | "" -> []//Sheet.findStackLeft pos state.Sheet
      | _ -> cell.Stack
    let txts =
      stack |> List.rev
      |> List.map (fun v ->
        match v with
        | Error (p, e) when pos = p ->  sprintf "(%s)" (errorMsg (p, e))
        | _ -> printValue v)
    let alert = List.exists (fun v -> match v with | Error (p, _) when p = pos -> true | _ -> false) stack
    let txt = System.String.Join(", ", txts)

    Html.td [
      prop.onClick (fun _ -> dispatch(StartEdit pos))
      prop.style (colourStyles cell)
      prop.children [
        Html.div [
          prop.style [style.display.flex] //; style.margin (length.em 0)]
          prop.children [
            Html.div [
              prop.style [
                style.textAlign.left
                style.flexGrow 1
                style.flexShrink 0
                style.fontWeight 500]
              prop.text cell.Input]
            Html.div [
              prop.style [
                style.textAlign.right
                style.lineHeight (length.percent 80)
                style.color "Gray"
                style.textDecoration.underline
                style.fontStyle.italic
                style.flexShrink 1
                style.whitespace.nowrap
                style.overflow.hidden
                style.textOverflow.ellipsis]
              prop.text txt]]]]]

  let renderCell dispatch pos state =
    match state.EditState, state.DisplayMode with
    | Some {Pos = p}, _ when p = pos -> renderCellEditor 1 dispatch state
    | _, Values -> renderValue dispatch pos state
    | _, Inputs -> renderInputs dispatch pos state

  let render state dispatch =

    let editBar = renderEditBar dispatch state

    let colLabel h =
      Html.th [
        prop.style [
          style.backgroundColor "LightGray"
          match state.EditState with
          | Some {Pos = Position (c, _)} when c = h ->
              style.borderBottomWidth 2; style.borderBottomColor "RoyalBlue"
          | _ -> ()
        ]
        prop.text (string h)]
    let rowLabel i =
      Html.th [
        prop.style [
          style.backgroundColor "LightGray"
          match state.EditState with
          | Some {Pos = Position (_, r)} when r = i ->
              style.borderRightWidth 2; style.borderRightColor "RoyalBlue"
          | _ -> ()
        ]

        prop.text (string i)]
    let colHeaders = Html.tr (topLeft::(state.Sheet.Cols |> List.map colLabel))

    let cells n =
      let cells = state.Sheet.Cols |> List.mapi (fun i h -> renderCell dispatch (Position (h, n)) state)
      (rowLabel n) :: cells
    let rows = state.Sheet.Rows |> List.map (cells >> Html.tr)

    Html.div [
      prop.style [style.margin 10; style.borderStyle.solid; style.borderWidth 1; style.borderColor "DarkGray"]
      prop.children [
        Html.table [
          prop.style [style.tableLayout.fixed']
          prop.classes [
            Bulma.Table
            Bulma.IsFullwidth
            Bulma.IsBordered
            Bulma.IsSize7
          ]
          prop.children [
            Html.thead [editBar; colHeaders]
            Html.tbody rows]
        ]
      ]
    ]

  let initialize () =
    { Sheet =
        { Cols = ['A' .. 'J']
          Rows = [1 .. 20]
          Definitions = Map.empty
          Cells = Map.empty }
      DisplayMode = Values
      EditState = None}, Cmd.Empty

