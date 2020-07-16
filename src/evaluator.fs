// BUGS
//  * need to delete bindings when def gets removed, kind of how unfill happens
//  * need to handle "shadowing" of bindings somehow
//  * need to add tests for the various primatives that can be run automatically
//  * need to add a tests that takes a map of the cells and then builds a new shee
//    adding the values in random order and ensuring the same value results.
//  * editing child cells should be an error

//  * Refactoring needed
//    - Edit state should not be part of the sheet, there should be a "State" type on
//      in the application

// ----------------------------------------------------------------------------
// LOGLO EVALUATOR
// Evaluator of Loglo language as defined by: Avi Bryant's http://loglo.app
// In part derivative of: http://tomasp.net/blog/2018/write-your-own-excel/
// ----------------------------------------------------------------------------
namespace Loglo


// ----------------------------------------------------------------------------
// DOMAIN
// ----------------------------------------------------------------------------
module Domain =
  open Parser

  type Position = Position of char * int
  module Position =
      let left  pos = match pos with Position (c, r) -> Position (char (int c-1), r)
      let right  pos = match pos with Position (c, r) -> Position (char (int c+1), r)
      let up  pos = match pos with Position (c, r) -> Position (c, r-1)
      let down  pos = match pos with Position (c, r) -> Position (c, r+1)


  type Point = {x: int; y: int}
  type Path = Point list
  type Paths = Path list

  type Value =
    | Binding of string
    | Code of Expr list
    | Error of Position*Error
    | Int of int
    | Name of string
    | Nil
    | Paths of Path list
    | Text of string
  and Error =
    | DivideByZero of int
    | DuplicateDefinition of string*Value
    | FilledCellOverwritten
    | InvalidParams of string*((Value*string) option list)*(Value list)
    | MissingDefinition of string
    | MissingItem of Position
    | NotImplemented of string
    | ParseFailure of string
    | ReadOnly
    | ReferenceCycle of Position list
    | ReferenceOffSheet of Position
    | SelfReference
    | StackUnderflow of int*int


  type CellType =
    | Input
    | Fill of (Value list)
    | Child

  type Cell = {
    Input: string
    Stack: Value list
    DependsOn: Position list
    Type: CellType
  }
  module Cell =
    let empty = {Input = ""; Stack = []; DependsOn = []; Type = Input}

    let push x cell =
      {cell with Stack = x::cell.Stack}

    let pop cell =
      match cell.Stack with
      | top::rest -> (top,{cell with Stack = rest})
      | _ -> (Nil, cell)

    let pop2 cell =
      let z,c' = pop cell
      let y,c'' = pop c'
      y, z, c''

    let pop3 cell =
      let z, c' = pop cell
      let y,c'' = pop c'
      let x,c''' = pop c''
      x, y, z, c'''

    let peek cell =
      match cell.Stack with
      | top::rest -> (top,cell)
      | _ -> (Nil, cell)

    let dup cell =
      match peek cell with
      | (Nil, c) -> c
      | v, c -> push v c

    let swap cell =
      let x,c = pop cell
      let y,c' = pop c
      push y (push x c')

    let drop cell =
      let _,c = pop cell
      c

    let value cell =
      let v, _ = peek cell
      v


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

  type Sheet = {
    Rows : int list
    Cols : char list
    EditState : EditState option
    DisplayMode: DisplayMode
    Cells : Map<Position, Cell>
    Definitions : Map<Value, Value>
  }
  module Sheet =
    open Position

    // Misc sheet manipulation functions
    let contains pos sheet =
      let (Position (c, r)) =  pos
      List.contains r sheet.Rows && List.contains c sheet.Cols

    let upsert pos sheet cell =
      {sheet with Cells = Map.add pos cell sheet.Cells}

    let tryFind pos sheet =
      match Map.tryFind pos sheet.Cells with
      | Some c -> Some c
      | None -> None

    let find pos sheet =
      match tryFind pos sheet with
      | Some c -> c
      | None -> Cell.empty

    let rec findStackLeft pos sheet =
      let pos' = Position.left pos
      if contains pos' sheet
      then
        match Map.tryFind pos' sheet.Cells with
        | Some c -> c.Stack
        //| None -> [] // If a blank cell should interrupt the stack
        | None -> findStackLeft pos' sheet
      else []

    let delete pos sheet =
      if Map.containsKey pos sheet.Cells
      then {sheet with Cells = Map.remove pos sheet.Cells}
      else sheet

    let error err pos sheet =
      upsert pos sheet (Cell.push (Error (pos, err)) (find pos sheet))

    let push value pos sheet =
      upsert pos sheet (Cell.push value (find pos sheet))

    // Private evaluation helpers
    let private unary pos sheet f =
      let cell = find pos sheet
      let z,c = Cell.pop cell
      let r = f z
      upsert pos sheet (Cell.push  r c)

    let private binary pos sheet f =
      let cell = find pos sheet
      let y, z, c = Cell.pop2 cell
      let r = f y z
      upsert pos sheet (Cell.push r c)

    let private ternary pos sheet f =
      let cell = find pos sheet
      let x, y, z, c = Cell.pop3 cell
      let r = f y z
      upsert pos sheet (Cell.push r c)

    // Stack & Language primatives
    let dup pos sheet =
      // TODO - error on empty stack
      let cell = find pos sheet
      upsert pos sheet (Cell.dup cell)

    let drop pos sheet =
      // TODO - error on empty stack
      let cell = find pos sheet
      upsert pos sheet (Cell.drop cell)

    let swap pos sheet =
      // TODO - error on stack with < 2 items
      let cell = find pos sheet
      upsert pos sheet (Cell.swap cell)

    let def pos sheet =
      let cell = find pos sheet
      match Cell.pop2 cell with
      | value, (Name n), c ->
        match Map.tryFind (Binding n) sheet.Definitions with
        | None ->
          let c' = Cell.push (Binding n) c
          {upsert pos sheet c' with Definitions = Map.add (Binding n) value sheet.Definitions}
        | Some v -> error (DuplicateDefinition (n, v)) pos sheet
      | value, _, c -> error (InvalidParams ("def", [Some ((Name "aName"), "")], [value])) pos sheet

    let fill pos sheet =
      let insertFilled parentPos pos sheet i =
        let (Position (c, r)) = parentPos
        let deps =
          if contains (left pos) sheet then [left pos]
          else []
        let cell = {
            Input = (sprintf "%c%i::%i" c r i);
            Stack = (findStackLeft pos sheet);
            DependsOn = parentPos::deps
            Type = Child}
        upsert pos sheet cell

      let (Position (col, row)) = pos
      let cell = find pos sheet
      let count, firstVal, otherVal, c = Cell.pop3 cell
      match count with
      | Int i when row + i > sheet.Rows.Length -> error (ReferenceOffSheet (Position (col, i + row))) pos sheet
      | Int i when i > 0 ->
          // Set cell type and push count back onto stack
          let cType = Fill (firstVal::(List.replicate (i - 1) otherVal))
          let sheet' = upsert pos sheet {Cell.push count c with Type = cType}
          // Fill the child cells
          [1..i]
          |> List.fold
              (fun (p, s) j -> down p, insertFilled pos p s j)
              (down pos, sheet')
          |> snd
      | _ -> error ((InvalidParams ("fill", [Some ((Int 0), "> 0"); None; None], [count; firstVal; otherVal]))) pos sheet

    // Arithmetic primatives
    let add pos sheet =
      let func y z =
        match y, z with
        | Int y, Int z -> Int (y+z)
        | Text y, Text z -> Text (y+z)
        | _ -> Error (pos, (InvalidParams ("Plus", [Some ((Int 0), ""); Some ((Int 0), "")], [y; z])))
      binary pos sheet func

    let sub pos sheet =
      let func y z =
        match y, z with
        | Int y, Int z -> Int (y-z)
        | _ -> Error (pos, (InvalidParams ("Sub", [Some ((Int 0), ""); Some ((Int 0), "")], [y; z])))
      binary pos sheet func

    let mul pos sheet =
      let func y z =
        match y, z with
        | Int y, Int z -> Int (y*z)
        | _ -> Error (pos, (InvalidParams ("Mul", [Some ((Int 0), ""); Some ((Int 0), "")], [y; z])))
      binary pos sheet func

    let div pos sheet =
      let func y z =
        match y, z with
        | Int y, Int 0 -> Error (pos, DivideByZero y)
        | Int y, Int z  -> Int (y/z)
        | _ -> Error (pos, (InvalidParams ("Div", [Some ((Int 0), ""); Some ((Int 0), "")], [y; z])))
      binary pos sheet func

    let neg pos sheet =
      let func z =
        match z with
        | Int i -> Int (-i)
        | _ -> Error (pos, (InvalidParams ("neg", [Some ((Int 0), "")], [z])))
      unary pos sheet func


// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------
module Evaluator =
  open Domain
  open Parser
  open Position
  open Sheet


  // Evaluate a single expression returning the result on the stack.
  let rec private evaluateExpr pos sheet e =
    match e with
    | Spaces _ -> sheet
    | TextExp s -> push (Text s) pos sheet
    | Block l -> push (Code l) pos sheet
    | Number i -> push (Int i) pos sheet
    | Reference r when not (contains (Position r) sheet) ->
        error (ReferenceOffSheet (Position r)) pos sheet
    | Reference r when Position r = pos ->
        error SelfReference pos sheet
    | Reference r ->
        let cycles pos sheet =
          let graph =
            sheet.Cells
            |> Map.toList
            |> List.map (fun (p, c) -> (p, c.DependsOn))
          Graph.cycles pos graph

        let cell = find pos sheet
        let newCell = {cell with DependsOn = ((Position r)::cell.DependsOn)}
        let newSheet = (upsert pos sheet newCell)
        // match find (Position r) shee with // If empty not an error
        match tryFind (Position r) newSheet with
        | None -> error (MissingItem (Position r)) pos newSheet
        | Some refCell ->
          match cycles pos newSheet with
          | h::_ -> error (ReferenceCycle h) pos sheet
          | _ ->
              // If a reference to a code block, recursivly evelaute otherwise
              // push the value.
              match Cell.value refCell with
              | Code exprList -> evaluateExprs exprList pos sheet
              | v -> push v pos newSheet
    | IndirectRef ->
        let (Position (c, i)) = (up pos)
        evaluateExpr pos sheet (Reference (c, i))
    | NestedRef (r, i) ->
        let parent = Sheet.find (Position r) sheet
        match parent.Type with
        | Fill l ->
            match List.tryItem (i-1) l with
            | Some v -> push v pos sheet
            | None -> delete pos sheet
        | _ -> delete pos sheet
    | Symbol s ->
        // Lookup up bindings in global dictionary. If not defined
        // then treat as primative.
        match Map.tryFind (Binding s) sheet.Definitions with
        | Some (Code exprList) -> evaluateExprs exprList pos sheet
        | Some v -> push v pos sheet
        | None -> evaluatePrimative pos sheet s
    | SymbolLiteral s -> push (Name s) pos sheet
    | Unparsable s -> error (ParseFailure s) pos sheet

  // Evaluate a list of expressions at pos starting with the given sheet.
  and private evaluateExprs exprList pos sheet =
    List.fold (evaluateExpr pos) sheet exprList

  // Evaluate the symbol as a primative operation
  and private evaluatePrimative pos sheet prim =
    match prim.ToString().ToLower() with
    // Stack & Language primatives
    | "dup" -> dup pos sheet
    | "drop" -> drop pos sheet
    | "swap" -> swap pos sheet
    | "def" -> def pos sheet
    | "fill" -> fill pos sheet
    // Arithmetic primatives
    | "+" -> add pos sheet
    | "-" -> sub pos sheet
    | "*" -> mul pos sheet
    | "/" -> div pos sheet
    | "neg" -> neg pos sheet
    // Geometry primatives
    | "close" -> error (NotImplemented "close") pos sheet
    | "intersect" -> error (NotImplemented "intersect") pos sheet
    | "line" -> error (NotImplemented "line") pos sheet
    | "rotate" -> error (NotImplemented "rotate") pos sheet
    | "scale" -> error (NotImplemented "scale") pos sheet
    | "union" -> error (NotImplemented "union") pos sheet

    | _ -> error (MissingDefinition prim) pos sheet

  // Parse new cell input and evaluate the parsed expressions. Set the stack to
  // the that of the cell to the left. Delete empty cells
  let private parseEvaluateCell pos sheet =
    let setStack pos sheet  =
      let cell = find pos sheet
      let stack = findStackLeft pos sheet
      upsert pos sheet {cell with Stack = stack}

    match parse (Sheet.find pos sheet).Input with
    | Ok [] -> delete pos sheet
    | Ok exprList ->
      evaluateExprs exprList pos (setStack pos sheet)
    | Result.Error (_, b) -> error (ParseFailure b) pos sheet

  // Calculate a new sheet by evaluating the new cell, then adding it to the
  // sheet and triggering a re-evaluation of all dependant cells. Keep track of
  // visited cells to prevent infinite recurssion.
  let rec private basicRecalc visited pos sheet =
    let dependants pos sheet =
      sheet.Cells
      |> Map.filter (fun _ c -> List.contains pos c.DependsOn)
      |> Map.map (fun p _ -> p)
      |> Map.toList |> List.map (fun (p,_) -> p)

    let sheet' = parseEvaluateCell pos sheet
    let visited' = Set.add pos visited
    dependants pos sheet'
    |> List.fold (fun s p ->
      if Set.contains p visited
      then s
      else basicRecalc (Set.add p visited') p s) sheet'

  // Public API - insert a new cell and either re-calculate the sheet or
  // just the active cell.
  let recalc pos sheet cellOnly input =
    let update pos sheet input =
      let deps =
        if contains (left pos) sheet then [left pos]
        else []
      upsert pos sheet {Input = input; Stack = []; DependsOn = deps; Type = Input}
    if cellOnly
      then
        let sheet' = parseEvaluateCell pos (update pos sheet input)
        let cell = Sheet.find pos sheet'
        match sheet.EditState with
        | Some es -> {sheet with EditState = Some {es with Pos = pos; Cell = {cell with Input = input}}}
        | None -> sheet
      else basicRecalc (Set.empty) pos (update pos sheet input)
