//TODO: 
// *  row select
// *  row insert
// *  highlight row/coll where selection is
// *  unfocused selection should be more obvious
// *  edit bar should be reactive columns so the input stack is continguous 
//    with the edit field and then contiguous with the output. 


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
    | UpdateCells of Position * string
    | UpdateEditValue of Position * string

  let update msg sheet =
    match msg with
    | StartEdit (pos, cell) ->
      { sheet with EditState = Some {Pos = pos; Cell = cell; Orig = cell; FullFocus = true} }, Cmd.Empty 
      // TODO - is this right or should these cells be arrowed over, or not be selectable at all?
      // match cell.Type with
      // | Child -> sheet, Cmd.Empty
      // | _ -> 
      //   { sheet with EditState = Some {Pos = pos; Cell = cell; FullFocus = true} }, Cmd.Empty

      // let sheet' = 
      //   match sheet.EditState with
      //   | Some {FullFocus = ff} when not ff -> { sheet with EditState = Some {Pos = pos; Cell = cell; FullFocus = false} }
      //   | Some _ -> sheet
      //   | None  -> { sheet with EditState = Some {Pos = pos; Cell = cell; FullFocus = false} }
      // sheet', Cmd.Empty
    | EndEdit cancel -> 
      match sheet.EditState, cancel with 
      | Some es, true ->
        {Sheet.upsert es.Pos sheet es.Orig with EditState = None}, Cmd.Empty
      | _, _ -> sheet, Cmd.Empty
    | ToggleDisplayMode -> 
      match sheet.DisplayMode with 
      | Inputs -> {sheet with DisplayMode = Values }, Cmd.Empty
      | Values -> {sheet with DisplayMode = Inputs}, Cmd.Empty
    | UpdateCells (pos, value) ->
        let sheet = Evaluator.recalc pos sheet false value
        sheet, Cmd.Empty
    | UpdateEditValue (pos, value) ->
        let sheet' = Evaluator.recalc pos sheet true value 
        sheet', Cmd.Empty


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
    // TODO - State machine for editing as follows:
    // * on click
    //    - no insertion point
    //    - pointer is arrow
    //    - field highlighted
    //    - all existing text is implicitly selected so typeing deletes it all
    //    - after typing insertion point is show
    //    - right/left arrow keys navigate out of cell
    // * on second click 
    //    - insertion point shown at point of click
    //    - pointer is bar
    //    - right/left arrow keys navigate text
    // NOTE: all the above is true for both the edit bar and the in cell editor

    let moveTo pos sheet = 
      if Sheet.contains pos sheet
      then 
        let c = Sheet.find pos sheet
        dispatch (StartEdit (pos, c))

    match sheet.EditState with
    | Some {Pos = pos; Cell = cell; FullFocus = ff} ->
      Html.td [
        prop.className [Bulma.IsSize7]
        prop.style [style.padding 0]
        prop.colSpan colSpan
        prop.children [
          Html.input [
            match cell.Type with 
            | Child -> prop.readOnly true
            | _ -> prop.readOnly false
            prop.className [Bulma.IsSize7; Bulma.Input; if colSpan = 1 then Bulma.IsFocused else ""]
            prop.style [style.borderRadius 0]
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
              | "ArrowLeft" when ff -> moveTo (Position.left pos) sheet
              | "ArrowRight" when ff -> moveTo (Position.right pos) sheet

              // HACK - otherwise cancells edit for some reason
              | " " when cell.Input.Length = 0 -> e.preventDefault()
              | _ -> () )

            prop.onClick (fun _ -> dispatch(StartEdit(pos, cell))) 
            prop.onTextChange (fun e -> 
              dispatch (UpdateCells(pos, e))) // dispatch on value accepted 
            prop.onInput (fun e ->            // dispatch on each keystroke
              let txt = (e.currentTarget :?> Browser.Types.HTMLInputElement).value
              dispatch(UpdateEditValue(pos, txt)))]]]
    | _ -> failwith "should not happen"

  let topLeft = Html.td [prop.style [style.width 45]]
  let renderEditBar dispatch sheet =
    match sheet.EditState with
    | Some {Pos = pos; Cell = cell; FullFocus = true} -> 
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
          prop.text (sprintf "[%s" txt)
          if alert then prop.style [style.color "Red"]]]//color "#Red"]]]
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
        match Cell.value cell with | Error _ -> prop.style [bgStyle; style.color "Red"] | _ -> prop.style[bgStyle]
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
