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
    | StartEdit active ->
        { sheet with Active = Some active }, Cmd.Empty
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
        sprintf "Err: need %i values but given %i." expected given
    | ParseFailure s -> sprintf "Err: %s is giberish" s
    | ReferenceCycle (Position(c,r)::t) ->
        let cycle =
          List.fold (fun s (Position (c, r)) -> s + (sprintf " to %c%i" c r)) "" t
        sprintf "Err: loop %c%i %s" c r cycle
    | SelfReference -> "Err: points to self"
    | DivideByZero i -> sprintf "Err: %i/0" i
    | MissingDefinition s ->
        sprintf "Err: %s is not defined" s
    | ReferenceOffSheet (Position (c, r)) ->
        sprintf "Err: %c%i is off the sheet" c r
    | InvalidParams (op, expected, given) ->
        let expected = System.String.Join(", ", List.map printType expected)
        let given = System.String.Join(", ", List.map printValue given)
        sprintf "Err: %s needs (%s) but given  (%s)." op expected given
    | MissingItem (Position (c, r)) -> sprintf "Err: %c%i is empty." c r
    | NotImplemented s -> sprintf "Err: %s is reserved by Loglo." s
    | FilledCellOverwritten -> "Err: read only"
    | _ -> e.ToString()


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

    match sheet.Active with
    | Some (pos, cell) ->
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
              printfn "%s" e.key
              let nextPos =
                match e.key with
                | "Enter" | "ArrowDown" -> Some (Position.down pos)
                | "Tab" -> e.preventDefault(); Some (Position.right pos)
                | "ArrowRight" -> Some (Position.right pos)
                | "ArrowLeft" -> Some (Position.left pos)
                | "ArrowUp" -> Some (Position.up pos)
                | "Space" -> printfn "space"; e.stopPropagation(); e.stopImmediatePropagation() ; None
                | _ -> None
              match nextPos with
              | Some p when Sheet.contains p sheet-> 
                let c = Sheet.find p sheet
                dispatch (StartEdit (p, c))
              | _ -> ())
            prop.onTextChange (fun e -> 
              dispatch (UpdateCells(pos, e))) // dispatch on value accepted 
            prop.onInput (fun e ->            // dispatch on each keystroke
              let txt = (e.currentTarget :?> Browser.Types.HTMLInputElement).value
              dispatch(UpdateActiveValue(pos, txt)))]]]
              // TODO - deleting last char ends edit
              // match txt.Trim() with
              //   | "" -> () // dispatching empty text ends edit
              //   | _ -> dispatch(UpdateActiveValue(pos, txt)))]]]
    | None -> failwith "should not happen"

  let topLeft = Html.td [prop.style [style.width 45]]
  let renderEditBar dispatch sheet =
    match sheet.Active with
    | Some (pos, cell) -> 
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
            |> List.map (fun v -> match v with | Error (p, e) -> sprintf "(%s)" (errorMsg (p, e)) | _ -> printValue v)
          let txt = System.String.Join(", ", txts)
          prop.colSpan 6
          prop.text (sprintf "[%s" txt)]]
      | None -> 
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
            if p = pos then errorMsg (p, e) else "Err"
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
    let cell = Sheet.find pos sheet
    if sheet.Active = Some (pos, cell) then 
      renderCellEditor 1 dispatch sheet
    else 
      renderValue dispatch pos cell

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
      Active = None
      Definitions = Map.empty
      Cells = Map.empty },
    Cmd.Empty
