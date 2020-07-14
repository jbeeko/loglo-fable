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
    | UpdateCells of Position * string
    | UpdateActiveValue of Position * string

  let update msg sheet =
    match msg with
    | StartEdit (pos, cell) ->
        { sheet with EditState = Some {Pos = pos; Cell = cell; FullFocus = true} }, Cmd.Empty

      // let sheet' = 
      //   match sheet.EditState with
      //   | Some {FullFocus = ff} when not ff -> { sheet with EditState = Some {Pos = pos; Cell = cell; FullFocus = false} }
      //   | Some _ -> sheet
      //   | None  -> { sheet with EditState = Some {Pos = pos; Cell = cell; FullFocus = false} }
      // sheet', Cmd.Empty
    | UpdateCells (pos, value) ->
        let sheet = Evaluator.recalc pos sheet false value
        sheet, Cmd.Empty
    | UpdateActiveValue (pos, value) ->
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
      | Code l, _ -> "{} "+cmnt
      | Paths l, _ -> "Paths "+cmnt
      | Error (p,e), _ -> "Error "+cmnt
      | Nil, _ -> "nil "+cmnt).Trim()

  let printValue v =
    match v with
    | Int i -> sprintf "%i" i
    | Text s -> sprintf "\"%s\"" s
    | Name s -> sprintf ":%s" s
    | Code l -> "{}"
    | Paths l -> sprintf "%i Paths" l.Length
    | Error (p,e) -> "Err"
    | Nil -> "nil"

  let errorMsg (_, e) =
    match e with
    | StackUnderflow (expected, given) ->
        "Underflow", sprintf "need %i params but given %i" expected given
    | ParseFailure s -> 
        "BadInput", sprintf "%s is giberish" s
    | ReferenceCycle (Position(c,r)::t) ->
        let cycle =
          List.fold (fun s (Position (c, r)) -> s + (sprintf " to %c%i" c r)) "" t
        "Loop", sprintf "%c%i %s" c r cycle
    | SelfReference -> 
        "SelfRef", "cell points to self"
    | DivideByZero i -> 
        "DivByZero", sprintf "%i/0" i
    | MissingDefinition s ->
        "Undefined", sprintf "%s is not defined" s
    | ReferenceOffSheet (Position (c, r)) ->
        "OffSheet", sprintf "%c%i is off the sheet" c r
    | InvalidParams (op, expected, given) ->
        let expected = System.String.Join(", ", List.map printType expected)
        let given = System.String.Join(", ", List.map printValue given)
        "InvalidParams", sprintf "%s needs (%s) but given  (%s)" op expected given
    | MissingItem (Position (c, r)) -> 
        "NoValue", sprintf "%c%i is empty" c r
    | NotImplemented s -> 
        "Reserved", sprintf "%s is reserved by Loglo" s
    | FilledCellOverwritten -> 
        "ReadOnly", "cell is read only"
    | _ -> "Err", e.ToString()



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

    // TODO - highlight row/col of selection
    // TODO - better emphasis of unfocused cell editor (border pixel width?)


    let moveTo pos sheet = 
      if Sheet.contains pos sheet
      then 
        let c = Sheet.find pos sheet
        dispatch (StartEdit (pos, c))

    match sheet.EditState with
    | Some {Pos = pos; Cell = cell; FullFocus = ff} ->
      Html.td [
        prop.style [style.padding 0]
        prop.colSpan colSpan
        prop.children [
          Html.input [
            prop.className [Bulma.Input]
            prop.style [style.borderRadius 0]
            prop.autoFocus true
            prop.type'.text
            prop.value cell.Input
            prop.onKeyDown (fun e -> 
              match e.key with
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
              | "ArrowLeft" when not ff -> moveTo (Position.left pos) sheet
              | "ArrowRight" when not ff -> moveTo (Position.right pos) sheet

              // HACK - otherwise cancells edit for some reason
              | " " when cell.Input.Length = 0 -> e.preventDefault()
              | _ -> () )

            prop.onClick (fun _ -> dispatch(StartEdit(pos, cell))) 
            prop.onTextChange (fun e -> 
              dispatch (UpdateCells(pos, e))) // dispatch on value accepted 
            prop.onInput (fun e ->            // dispatch on each keystroke
              let txt = (e.currentTarget :?> Browser.Types.HTMLInputElement).value
              dispatch(UpdateActiveValue(pos, txt)))]]]
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
              | Error (p, e) when pos = p-> 
                let c, d = errorMsg (p, e)
                sprintf "(%s: %s)" c d
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


  let renderValue dispatch pos cell =
    // TODO - should draw Paths as SVG on a canvas, others as str
    // TODO - split cell, with stack and value below, any SVG above
    //        This will give more context.
    let content cell =
      let value =
        match (Cell.value cell) with
        | Int i as v -> printValue v
        | Text s as v -> printValue v
        | Code _ as v -> printValue v
        | Name s as v -> printValue v
        | Paths p as v -> printValue v
        | Error (p, e) ->
            if p = pos 
            then fst (errorMsg (p, e))
            else "Err"
        | Nil -> ""
      value

    Html.td [
        prop.style [
          match Cell.value cell, cell.Type with
          | Error _, _ -> style.backgroundColor "#FFB0B0"
          | _, Child-> style.backgroundColor "AliceBlue"
          | Nil, _ -> style.backgroundColor "White"
          | _, _-> style.backgroundColor "LightYellow"]
        prop.onClick (fun _ -> dispatch(StartEdit(pos, cell))) 
        prop.text (content cell) ]

  let renderCell dispatch pos sheet =
    match sheet.EditState with
    | Some {Pos = p} when p = pos -> renderCellEditor 1 dispatch sheet
    | _ -> renderValue dispatch pos (Sheet.find pos sheet) 

  let render sheet dispatch =

    let editBar = renderEditBar dispatch sheet

    let colLabel (h:string) = Html.th [prop.style [style.backgroundColor "LightGray"];  prop.text h]
    let rowLabel (h:string) = Html.th [prop.style [style.textAlign.right; style.backgroundColor "LightGray"];  prop.text h]
    let colHeaders = Html.tr (topLeft::(sheet.Cols |> List.map (string >> colLabel)))

    let cells n =
      let cells = sheet.Cols |> List.mapi (fun i h -> renderCell dispatch (Position (h, n)) sheet)
      rowLabel (string n) :: cells
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
      Definitions = Map.empty
      Cells = Map.empty },
    Cmd.Empty
