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
    | StartEdit of Position
    | UpdateValue of Position * string

  let update msg sheet =
    match msg with
    | StartEdit(pos) ->
        { sheet with Active = Some pos }, Cmd.Empty
    | UpdateValue(pos, value) ->
        let sheet = Evaluator.recalc pos sheet value
        sheet, Cmd.Empty


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
    | Error (p,e) -> "Error"
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
        sprintf "%s needs (%s) but given  (%s)." op expected given
    | MissingItem (Position (c, r)) -> sprintf "Err: %c%i is empty." c r
    | NotImplemented s -> sprintf "Err: %s is reserved by Loglo." s
    | FilledCellOverwritten -> "Err: read only"
    | _ -> e.ToString()

  let renderEditor dispatch pos (value:string) =
    Html.td [
      prop.style [style.padding 0]
      prop.children [
        Html.input [
          prop.className [Bulma.Input]
          prop.style [style.borderRadius 0]
          prop.autoFocus true
          prop.type'.text
          prop.onTextChange (fun e -> dispatch (UpdateValue(pos, e))) // only update on exit
          //prop.onInput (fun e -> dispatch(UpdateValue(pos, (e.currentTarget :?> Browser.Types.HTMLInputElement).value)))
          prop.value value ]
      ]
    ]

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
        prop.onClick (fun _ -> dispatch(StartEdit(pos)) ) 
        prop.text (content cell) ]

  let renderCell dispatch pos sheet =
    let cell = Sheet.find pos sheet
    if sheet.Active = Some pos then 
      renderEditor dispatch pos cell.Input
    else 
      renderValue dispatch pos cell

  let render sheet dispatch =
    let topLeft = Html.td [prop.style [style.width 40]]
    let colLabel (h:string) = Html.th [prop.style [style.backgroundColor "LightGray"];  prop.text h]
    let rowLabel (h:string) = Html.th [prop.style [style.textAlign.right; style.backgroundColor "LightGray"];  prop.text h]
    let colHeaders = Html.tr (topLeft::(sheet.Cols |> List.map (string >> colLabel)))
    let editHeaders = Html.tr [
        topLeft
        Html.td [prop.text "In: [1 2 3]"; prop.colSpan 2]
        Html.td [
          prop.style [style.padding 0]
          prop.colSpan (sheet.Cols.Length - 4)
          prop.children [
            Html.input [
              prop.className [Bulma.Input]
              prop.style [style.borderRadius 0]
              prop.autoFocus true
              prop.type'.text
              //prop.onTextChange (fun e -> dispatch (UpdateValue(pos, e))) // only update on exit
              //prop.onInput (fun e -> dispatch(UpdateValue(pos, (e.currentTarget :?> Browser.Types.HTMLInputElement).value)))
              //prop.value value 
              ]
          ]
          ]
        Html.td [prop.text "Out: [1 5]"; prop.colSpan 2]]

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
            Html.thead [editHeaders; colHeaders]
            Html.tbody rows]
        ]
      ]
    ]

  let initialize () =
    { Cols = ['A' .. 'J']
      Rows = [1 .. 10]
      Active = None
      Definitions = Map.empty
      Cells = Map.empty },
    Cmd.Empty
