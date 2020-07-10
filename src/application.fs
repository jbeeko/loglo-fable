// ----------------------------------------------------------------------------
// LOGLO UI
// Loglo UI as demonstrated by: Avi Bryant's loglo.app
// In part derivative of: http://tomasp.net/blog/2018/write-your-own-excel/
namespace Loglo


module Application =

  open Elmish
  open Feliz

  //open Elmish.React
  //open Fable.Helpers.React
  //open Fable.Helpers.React.Props
  //open Fable.Core.JsInterop



  open Domain


  // ----------------------------------------------------------------------------
  // EVENT HANDLING
  // ----------------------------------------------------------------------------
  type Event =
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

  let errorMsg (p, e) =
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

  // let renderEditor (trigger:Event -> unit) pos value =
  //   td [ Class "selected"] [
  //     input [
  //       AutoFocus true
  //       OnChange ignore
  //       OnInput (fun e -> trigger(UpdateValue(pos, e.target?value)))
  //       Value value ]
  //   ]

  // let renderValue trigger pos (cell: Cell) =
  //   // TODO - should draw Paths as SVG on a canvas, others as str
  //   // TODO - split cell, with stack and value below, any SVG above
  //   //        This will give more context.
  //   let content cell =
  //     let value =
  //       match (Cell.value cell) with
  //       | Int i as v -> printValue v
  //       | Text s as v -> printValue v
  //       | Code _ as v -> printValue v
  //       | Name s as v -> printValue v
  //       | Paths p as v -> printValue v
  //       | Error (p, e) ->
  //           if p = pos then errorMsg (p, e) else "ERR"
  //       | Nil -> ""
  //     value

  //   td
  //     [
  //       Style (
  //         match Cell.value cell, cell.Type with
  //         | Error _, _ -> [Background "#FFB0B0"]
  //         | _, Child-> [Background "AliceBlue"]
  //         | Nil, _ -> [Background "White"]
  //         | _, _-> [Background "LightYellow"])
  //       OnClick (fun _ -> trigger(StartEdit(pos)) ) ]
  //     [ str (content cell) ]

  // let renderCell trigger pos sheet =
  //   let cell = Sheet.find pos sheet
  //   if sheet.Active = Some pos then
  //     renderEditor trigger pos cell.Input
  //   else
  //     renderValue trigger pos cell

  // let render sheet trigger =
  //   let empty = td [] []
  //   let header h = th [Style ([Background "#D3D3D3"])] [str h]
  //   let headers = sheet.Cols |> List.map (fun h -> header (string h))
  //   let headers = empty::headers

  //   let cells n =
  //     let cells = sheet.Cols |> List.mapi (fun i h -> renderCell trigger (Position (h, n)) sheet)
  //     header (string n) :: cells
  //   let rows = sheet.Rows |> List.map (fun r -> tr [] (cells r))

  //   table [] [
  //     tr [] headers
  //     tbody [] rows
  //   ]

// open Feliz
// open Elmish

// type State = { Count: int }

// type Msg =
//     | Increment
//     | Decrement

// let init() = { Count = 0 }, Cmd.none

// let update (msg: Msg) (state: State) =
//     match msg with
//     | Increment -> { state with Count = state.Count + 1 }, Cmd.none
//     | Decrement -> { state with Count = state.Count - 1 }, Cmd.none

  let render (sheet: Sheet) (trigger: Event -> unit) =
    Html.div [
      Html.button [
          //prop.onClick (fun _ -> dispatch Increment)
          prop.text "Increment"
      ]

      Html.button [
          //prop.onClick (fun _ -> dispatch Decrement)
          prop.text "Decrement"
      ]

      //Html.h1 state.Count
    ]


  let initialize () =
    { Cols = ['A' .. 'E']
      Rows = [1 .. 5]
      Active = None
      Definitions = Map.empty
      Cells = Map.empty },
    Cmd.Empty
