namespace Loglo


// ----------------------------------------------------------------------------
// GRAPH
// http://www.fssnip.net/av/title/NinetyNine-F-Problems-Problems-80-89-Graphs
// ----------------------------------------------------------------------------
module Graph =
  type Edge<'a> = 'a * 'a
  type Graph<'a> = list<'a> * Edge<'a> list
  type Node<'a> = 'a * 'a list
  type AdjacencyGraph<'a> = Node<'a> list

  let cycles start (g: AdjacencyGraph<'a>) =
    let map = g |> Map.ofList
    let rec loop route visited = [
      let current = List.head route
      for next in Option.defaultValue [] (Map.tryFind current map) do
        if next = start then
          yield List.rev <| next::route
        if not (Set.contains next visited) then
          yield! loop (next::route) (Set.add next visited)
    ]

    loop [start] <| Set.singleton start
