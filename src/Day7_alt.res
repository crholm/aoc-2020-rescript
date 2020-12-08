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


type edge = {
  name: string,
  weight: int,
}
type graph = {
  forward: SMap.t<list<edge>>,
  reverse: SMap.t<list<edge>>,
}

let children = (key, g) => {
  getOr(key, list{}, g.forward)
}
let parents = (key, g) => {
  getOr(key, list{}, g.reverse)
}



let parse_children = a => {
  switch a {
  | "no other" => list{}
  | _ => a |> AOC.str_split(", ") |> List.map(s => {
      let parts = s |> AOC.str_split(" ")
      let weight = parts |> List.hd |> int_of_string
      let name = parts |> List.tl |> AOC.str_join(" ")
      {name: name, weight: weight}
    })
  }
}

let g = Data.str |> AOC.str_split("\n") |> List.map(l => {
  let a =
    l
    |> AOC.str_replace(%re("/\\./g"), "")
    |> AOC.str_replace(%re("/ bags/g"), "")
    |> AOC.str_replace(%re("/ bag/g"), "")
    |> AOC.str_split(" contain ")
  let node = a->List.nth(0)
  let children = a->List.nth(1) |> parse_children
  (node, children)

}) |> List.fold_left((g, (parent, children)) => {
  let f = g.forward |> SMap.add(parent, children)
  let r = children |> List.fold_left((acc, child) => {
    let _new = acc |> getOr(child.name, list{}) |> List.append(list{{name: parent, weight: child.weight}})
    acc |> SMap.add(child.name, _new)
  }, g.reverse)
  {forward: f, reverse: r}
}, {forward: SMap.empty, reverse: SMap.empty})



// // Part 1
let rec allParents = (g, l) => {
  l |> List.map(c => parents(c.name, g) |> allParents(g) ) |> List.flatten |> List.append(l)
}

parents("shiny gold", g)
|> allParents(g)
|> List.sort_uniq((a, b) => Pervasives.compare(a.name, b.name))
|> List.length
|> Js.log2("1 >")

// // Part 2
let rec bagContent = (g, l) => {
  l |> List.fold_left((acc, bag) => {
    acc + bag.weight + bag.weight * bagContent(g, children(bag.name, g))
  }, 0)
}
children("shiny gold", g) |> bagContent(g) |> Js.log2("2 >")
