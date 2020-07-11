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
        sprintf "Need %i values but given %i." expected given
    | ParseFailure s -> sprintf "%s is not understood." s
    | ReferenceCycle (Position(c,r)::t) ->
        let cycle =
          List.fold (fun s (Position (c, r)) -> s + (sprintf " to %c%i" c r)) "" t
        sprintf "Loop: %c%i %s" c r cycle
    | SelfReference -> "Self reference."
    | DivideByZero i -> sprintf "%i is divided by zero." i
    | MissingDefinition s ->
        sprintf "The %s is not a Loglo word nor a user defined one." s
    | ReferenceOffSheet (Position (c, r)) ->
        sprintf "Position %c%i is off the sheet." c r
    | InvalidParams (op, expected, given) ->
        let expected = System.String.Join(", ", List.map printType expected)
        let given = System.String.Join(", ", List.map printValue given)
        sprintf "%s needs (%s) but given  (%s)." op expected given
    | MissingItem (Position (c, r)) -> sprintf "%c%i is empty." c r
    | NotImplemented s -> sprintf "%s is reserved by Loglo." s
    | FilledCellOverwritten -> "Filled cells can't be edited."
    | _ -> e.ToString()

  let renderEditor dispatch pos (value:string) =
    Html.td [
      Html.input [
        prop.className ["selected"]
        prop.autoFocus true
        prop.onChange (fun (e:string) -> dispatch (UpdateValue(pos, e))) // only update on exit
        //prop.onInput (fun e -> dispatch(UpdateValue(pos, (e.currentTarget :?> Browser.Types.HTMLInputElement).value)))
        prop.value value ]
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
            if p = pos then errorMsg (p, e) else "ERR"
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
    let empty = Html.td []
    let header (h:string) = Html.th [prop.style [style.backgroundColor "#D3D3D3"];  prop.text h]
    let headers = sheet.Cols |> List.map (string >> header) 
    let headers = empty::headers

    let cells n =
      let cells = sheet.Cols |> List.mapi (fun i h -> renderCell dispatch (Position (h, n)) sheet)
      header (string n) :: cells
    let rows = sheet.Rows |> List.map (cells >> Html.tr) 

    Html.table  [
      Html.thead [Html.tr headers]
      Html.tbody rows
    ]

  let initialize () =
    { Cols = ['A' .. 'E']
      Rows = [1 .. 5]
      Active = None
      Definitions = Map.empty
      Cells = Map.empty },
    Cmd.Empty
