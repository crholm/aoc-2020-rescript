module Data = Data_Day7
module AOC = AOC
AOC.print_header(7)

let parse_edges = a => {
  if a |> List.tl |> List.hd == "no other" {
    list{}
  } else {
    a |> List.tl |> List.hd |> AOC.str_split(", ") |> List.map(s => {
      let weight = s |> AOC.str_split(" ") |> List.hd |> int_of_string
      let name =
        s |> AOC.str_split(" ") |> List.tl |> List.fold_left((acc, str) => acc ++ " " ++ str, "")
      (name->String.sub(1, String.length(name) - 1), weight)
    })
  }
}

module SMap = Map.Make({
  type t = string
  let compare = (a: string, b: string) => a == b ? 0 : a < b ? -1 : 1
})

let graph = Data.str |> AOC.str_split("\n") |> List.map(l => {
  let a =
    l
    |> AOC.str_replace(%re("/\\./g"), "")
    |> AOC.str_replace(%re("/ bags/g"), "")
    |> AOC.str_replace(%re("/ bag/g"), "")
    |> AOC.str_split(" contain ")
  let node = a |> List.hd
  let edges = a |> parse_edges
  (node, edges)
}) |> List.fold_left((acc, pair) => {
  let (node, edges) = pair
  SMap.add(node, edges, acc)
}, SMap.empty)

let rev = SMap.fold((key: string, v, acc) => {
  v |> List.fold_left((acc2, edge) => {
    let (name, _) = edge
    let now = if SMap.mem(name, acc2) {
      SMap.find(name, acc2)
    } else {
      list{}
    }
    let res = now |> List.append(list{key})
    SMap.add(name, res, acc2)
  }, acc)
}, graph, SMap.empty)

let rec getList = n => {
  if n |> List.length == 0 {
    list{}
  } else {
    n |> List.map(c => {
      if SMap.mem(c, rev) {
        SMap.find(c, rev) |> getList
      } else {
        list{}
      }
    }) |> List.flatten |> List.append(n)
  }
}

SMap.find("shiny gold", rev)
|> getList
|> List.sort_uniq(Pervasives.compare)
|> List.length
|> Js.log2("1 >")

let rec numOfBags = l => {
  if List.length(l) == 0 {
    1
  } else {
    l |> List.fold_left((acc, edge) => {
      let (name, weight) = edge
      weight * numOfBags(SMap.find(name, graph)) + acc
    }, 1)
  }
}

SMap.find("shiny gold", graph) |> numOfBags |> -1->\"+" |> Js.log2("2 >")
//126
