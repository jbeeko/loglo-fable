//TODO:
// *  row select
// *  row insert
// *  highlight row/coll where selection is
// *  unfocused selection should be more obvious
// *  edit bar should be reactive columns so the input stack is continguous
//    with the edit field and then contiguous with the output.
//  * Arrow down though filled cells shits the sheet up
//  * Refactoring needed
//    - Edit state should not be part of the sheet, there should be a "State" type on
//      in the application that manages state of the application
//      Now just edit state, but in the future things like hidden rows etc.


// ----------------------------------------------------------------------------
// LOGLO UI
// Loglo UI as demonstrated by: Avi Bryant's loglo.app
// In part derivative of: http://tomasp.net/blog/2018/write-your-own-excel/
namespace Loglo


module Application =
  open Elmish
  open Feliz

  open Domain


  // ----------------------------------------------------------------------------
  // MESSAGE HANDLING
  // ----------------------------------------------------------------------------
  type Message =
    | StartEdit of (Position*Cell)
    | EndEdit of bool
    | ToggleDisplayMode
    | UpdateCell of Position * string
    | UpdateEditValue of Position * string

  let update msg sheet =
    match msg with
    | StartEdit (pos, cell) ->
      //{ sheet with EditState = Some {Pos = pos; Cell = cell; Orig = cell; FullFocus = true} }, Cmd.Empty
      // TODO - is this right or should these cells be arrowed over, or not be selectable at all?
      // match cell.Type with
      // | Child -> sheet, Cmd.Empty
      // | _ ->
      //   { sheet with EditState = Some {Pos = pos; Cell = cell; FullFocus = true} }, Cmd.Empty

      let sheet' =
        match sheet.EditState with
        | Some es when es.Pos = pos && es.Focus <> FullFocus ->
          { sheet with EditState = Some {es with Focus = FullFocus} }
        | Some es when es.Pos = pos && es.Focus = FullFocus -> sheet
        | _  ->
          { sheet with EditState = Some {Pos = pos; Cell = cell; Orig = cell; Focus = Initial} }
      sheet', Cmd.Empty
    | EndEdit cancel ->
      match sheet.EditState, cancel with
      | Some es, true ->
        {Sheet.upsert es.Pos sheet es.Orig with EditState = None}, Cmd.Empty
      | _, _ -> sheet, Cmd.Empty
    | ToggleDisplayMode ->
      match sheet.DisplayMode with
      | Inputs -> {sheet with DisplayMode = Values }, Cmd.Empty
      | Values -> {sheet with DisplayMode = Inputs}, Cmd.Empty
    | UpdateCell (pos, value) ->
        let sheet = Evaluator.recalc pos sheet false value
        sheet, Cmd.Empty
    | UpdateEditValue (pos, value) ->
        let sheet' = Evaluator.recalc pos sheet true value
        let editState =
          match sheet'.EditState with
          | Some es when es.Focus = Initial -> Some {es with Focus = PartialFocus}
          | es -> es
        {sheet' with EditState = editState}, Cmd.Empty


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



  let renderCellEditor colSpan dispatch sheet =
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

    // TODO Delete key blanks existing content but on leaving the content is still there.


    // TODO not getting cursor in edit bar
    let moveTo pos sheet =
      if Sheet.contains pos sheet
      then
        let c = Sheet.find pos sheet
        dispatch (StartEdit (pos, c))

    match sheet.EditState with
    | Some {Pos = pos; Cell = cell; Focus = focus} ->
      Html.td [
        prop.className [Bulma.IsSize7]
        prop.style [style.padding 0]
        prop.colSpan colSpan
        prop.children [
          Html.input [
            // prop.readOnly (cell.Type = Child) // scrolls sheet!?
            prop.className [Bulma.IsSize7; Bulma.Input; if colSpan = 1 then Bulma.IsFocused else ""]
            match sheet.EditState with
            | Some es when es.Focus = Initial || es.Focus = PartialFocus ->
              prop.style [style.borderRadius 0; (Interop.mkStyle "caret-color" "transparent"); style.cursor.defaultCursor]
            | _ -> prop.style [style.borderRadius 0]
            prop.autoFocus true
            prop.type'.text
            prop.value cell.Input
            prop.onKeyDown (fun e ->
              match e.key with
              | "`" when e.ctrlKey -> dispatch ToggleDisplayMode
              | "Escape" -> dispatch (EndEdit true)

              | "Enter" when e.shiftKey ->moveTo (Position.up pos) sheet
              | "Enter" -> moveTo (Position.down pos) sheet

              | "Tab" when e.shiftKey ->
                e.preventDefault()
                moveTo (Position.left pos) sheet
              | "Tab" ->
                e.preventDefault()
                moveTo (Position.right pos) sheet

              | "ArrowUp" -> moveTo (Position.up pos) sheet
              | "ArrowDown" -> moveTo (Position.down pos) sheet
              | "ArrowLeft" when focus <> FullFocus -> moveTo (Position.left pos) sheet
              | "ArrowRight" when focus <> FullFocus -> moveTo (Position.right pos) sheet

              // Initial key stroke clears existing contact (as in Excel and numbers)
              // Don't clear for function, option alt etc.
              | key when key.Length = 1 || key = "Backspace" ->
                match sheet.EditState with
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

            prop.onClick (fun e -> dispatch(StartEdit(pos, cell)))
            prop.onTextChange (fun e ->
              dispatch (UpdateCell(pos, e))) // dispatch on value accepted
            prop.onInput (fun e ->            // dispatch on each keystroke
              let txt = (e.currentTarget :?> Browser.Types.HTMLInputElement).value
              dispatch(UpdateEditValue(pos, txt)))]]]
    | _ -> failwith "should not happen"

  let topLeft = Html.td [prop.style [style.width 45]]
  let renderEditBar dispatch sheet =
    match sheet.EditState with
    | Some {Pos = pos; Cell = cell} ->
      Html.tr [
        topLeft
        // Pre stack
        Html.td [
          let stack = Sheet.findStackLeft pos sheet
          let txt = System.String.Join(", ", stack |> List.rev |> List.map printValue)
          prop.colSpan 1
          prop.text (sprintf "[%s" txt)
          prop.style [style.textAlign.right]]

        renderCellEditor (sheet.Cols.Length - 7) dispatch sheet

        // Post stack
        Html.td [
          let stack =
            match cell.Input.Trim() with
            | "" -> Sheet.findStackLeft pos sheet
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
          Html.td [prop.colSpan (sheet.Cols.Length - 7)]
          Html.td [prop.text "[ "; prop.colSpan 6]]


  let renderValue dispatch pos sheet =
    // TODO - should draw Paths as SVG on a canvas, others as str
    // TODO - split cell, with stack and value below, any SVG above
    //        This will give more context.
    let cell = (Sheet.find pos sheet)
    let content cell =
      let value =
        match (Cell.value cell) with
        | Int i as v -> printValue v
        | Text s as v -> printValue v
        | Code _ as v -> printValue v
        | Name s as v -> printValue v
        | Binding s as v -> printValue v
        | Paths p as v -> printValue v
        | Error (p, e) ->
            if p = pos
            then errorMsg (p, e)
            else "Err"
        | Nil -> ""
      value
    let bgStyle =
        match Cell.value cell, cell.Type with
        | _, Child-> style.backgroundColor "AliceBlue"
        | Nil, _ -> style.backgroundColor "White"
        | _, _-> style.backgroundColor "LightYellow"

    Html.td [
        match Cell.value cell with
          | Error _ -> prop.style [bgStyle; style.color "Red"]
          | _ -> prop.style[bgStyle]
        prop.onClick (fun _ -> dispatch(StartEdit(pos, cell)))
        prop.text (match sheet.DisplayMode with | Values -> content cell | Inputs -> cell.Input)]

  let renderCell dispatch pos sheet =
    match sheet.EditState with
    | Some {Pos = p} when p = pos -> renderCellEditor 1 dispatch sheet
    | _ -> renderValue dispatch pos sheet

  let render sheet dispatch =

    let editBar = renderEditBar dispatch sheet

    let colLabel h =
      Html.th [
        match sheet.EditState with
        | Some {Pos = Position (c, _)} when c = h ->
          prop.style [style.backgroundColor "LightGray"; style.borderBottomWidth 2; style.borderBottomColor "RoyalBlue"]
        | _ -> prop.style [style.backgroundColor "LightGray"]
        prop.text (string h)]
    let rowLabel i =
      Html.th [
        match sheet.EditState with
        | Some {Pos = Position (_, r)} when r = i ->
          prop.style [style.backgroundColor "LightGray"; style.borderRightWidth 2; style.borderRightColor "RoyalBlue"]
        | _ -> prop.style [style.backgroundColor "LightGray"]
        prop.text (string i)]
    let colHeaders = Html.tr (topLeft::(sheet.Cols |> List.map colLabel))

    let cells n =
      let cells = sheet.Cols |> List.mapi (fun i h -> renderCell dispatch (Position (h, n)) sheet)
      (rowLabel n) :: cells
    let rows = sheet.Rows |> List.map (cells >> Html.tr)

    Html.div [
      prop.style [style.margin 10; style.borderStyle.solid  ; style.borderWidth 1; style.borderColor "DarkGray"]
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
    { Cols = ['A' .. 'J']
      Rows = [1 .. 20]
      EditState = None
      DisplayMode = Values
      Definitions = Map.empty
      Cells = Map.empty },
    Cmd.Empty
