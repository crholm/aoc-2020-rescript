module Data = Data_Day7
module AOC = AOC
AOC.print_header(7)

module SMap = Map.Make({
  type t = string
  let compare = Pervasives.compare
})

let parse_edges = a => {
  switch a {
  | "no other" => list{}
  | _ => a |> AOC.str_split(", ") |> List.map(s => {
      let parts = s |> AOC.str_split(" ")
      let weight = parts |> List.hd |> int_of_string
      let name = parts |> List.tl |> AOC.str_join(" ")
      (name, weight)
    })
  }
}

let graph = Data.str |> AOC.str_split("\n") |> List.map(l => {
  let a =
    l
    |> AOC.str_replace(%re("/\\./g"), "")
    |> AOC.str_replace(%re("/ bags/g"), "")
    |> AOC.str_replace(%re("/ bag/g"), "")
    |> AOC.str_split(" contain ")
  let node = a->List.nth(0)
  let edges = a->List.nth(1) |> parse_edges
  (node, edges)
}) |> List.fold_left((acc, (node, edges)) => {
  SMap.add(node, edges, acc)
}, SMap.empty)

// Part 1
let rev = SMap.fold((key: string, v, acc) => {
  v |> List.fold_left((acc2, (name, _)) => {
    let now = SMap.mem(name, acc2) ? SMap.find(name, acc2) : list{}

    let res = now |> List.append(list{key})
    SMap.add(name, res, acc2)
  }, acc)
}, graph, SMap.empty)

let smap_find = (key, map) => {
  SMap.mem(key, map) ? Some(SMap.find(key, rev)) : None
}
let rec getList = n => {
  n |> List.map(c => {
    switch smap_find(c, rev) {
    | Some(v) => v |> getList
    | None => list{}
    }
  }) |> List.flatten |> List.append(n)
}

SMap.find("shiny gold", rev)
|> getList
|> List.sort_uniq(Pervasives.compare)
|> List.length
|> Js.log2("1 >")

// Part 2

let rec numOfBags = l => {
  l |> List.fold_left((acc, (name, weight)) => {
    acc + weight + weight * numOfBags(SMap.find(name, graph))
  }, 0)
}
SMap.find("shiny gold", graph) |> numOfBags |> Js.log2("2 >")
