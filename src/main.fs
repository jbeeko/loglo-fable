namespace Loglo
module Main =

  open Fable.Core.JsInterop

  importAll "../styles/main.scss"

  open Elmish
  open Elmish.React
  //open Elmish.Debug
  open Elmish.HMR

  // ----------------------------------------------------------------------------
  // LOGLO ENTRY POINT
  // ----------------------------------------------------------------------------
  Program.mkProgram Application.initialize Application.update Application.render
  // #if DEBUG
  // |> Program.withDebugger
  // #endif
  |> Program.withReactSynchronous "loglo"
  |> Program.run