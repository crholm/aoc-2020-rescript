// https://adventofcode.com/2020/day/7

module Data = Data_Day7
module AOC = AOC
AOC.print_header("7, alt")

module SMap = Map.Make({
  type t = string
  let compare = Pervasives.compare
})
let getOr = (key, default, m) => {
  SMap.mem(key, m) ? SMap.find(key, m) : default
}

module Graph = {
  type edge = {
    name: string,
    weight: int,
  }
  type t = {
    forward: SMap.t<list<edge>>,
    reverse: SMap.t<list<edge>>,
  }

  let children = (key, g) => {
    getOr(key, list{}, g.forward)
  }
  let parents = (key, g) => {
    getOr(key, list{}, g.reverse)
  }
  let add_edge = (node, edge, g) => {
    let ff =
      g.forward
      |> getOr(node, list{})
      |> List.append(list{edge})
      |> List.sort_uniq((a, b) => Pervasives.compare(a, b))

    let rr =
      g.reverse
      |> getOr(edge.name, list{})
      |> List.append(list{{name: node, weight: edge.weight}})
      |> List.sort_uniq((a, b) => Pervasives.compare(a, b))

    {forward: SMap.add(node, ff, g.forward), reverse: SMap.add(edge.name, rr, g.reverse)}
  }
  let empty = {forward: SMap.empty, reverse: SMap.empty}
}

let parse_children = a => {
  switch a {
  | "no other" => list{}
  | _ => a |> AOC.str_split(", ") |> List.map(s => {
      let parts = s |> AOC.str_split(" ")
      let weight = parts |> List.hd |> int_of_string
      let name = parts |> List.tl |> AOC.str_join(" ")
      ({name: name, weight: weight}: Graph.edge)
    })
  }
}

let clean_line = s => {
  s
  |> AOC.str_replace(%re("/\\./g"), "")
  |> AOC.str_replace(%re("/ bags/g"), "")
  |> AOC.str_replace(%re("/ bag/g"), "")
}

// Constructing graph
let g = Data.str |> AOC.str_split("\n") |> List.map(l => {
  let a = l |> clean_line |> AOC.str_split(" contain ")
  let parent = a->List.nth(0)
  let children = a->List.nth(1) |> parse_children
  (parent, children)
}) |> List.fold_left((g, (parent, children)) => {
  children |> List.fold_left(Graph.add_edge(parent)->AOC.frev, g)
}, Graph.empty)

// // Part 1
let rec allParents = (g, l) => {
  l |> List.map((c: Graph.edge) => {
    Graph.parents(c.name, g) |> allParents(g)
  }) |> List.flatten |> List.append(l)
}

Graph.parents("shiny gold", g)
|> allParents(g)
|> List.sort_uniq((a: Graph.edge, b: Graph.edge) => Pervasives.compare(a.name, b.name))
|> List.length
|> Js.log2("1 >")

// // Part 2
let rec bagContent = (g, l) => {
  l |> List.fold_left((acc, bag: Graph.edge) => {
    acc + bag.weight + bag.weight * bagContent(g, Graph.children(bag.name, g))
  }, 0)
}

Graph.children("shiny gold", g) |> bagContent(g) |> Js.log2("2 >")
