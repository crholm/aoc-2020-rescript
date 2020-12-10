// https://adventofcode.com/2020/day/6

module Data = Data_Day6
module AOC = AOC

AOC.print_header(6)

//Part 1
Data.str
|> AOC.str_split("\n\n")
|> List.map(AOC.str_replace(%re("/\\n/g"), ""))
|> List.map(AOC.charlist_of_string)
|> List.map(List.sort_uniq(Pervasives.compare))
|> List.map(List.length)
|> List.fold_left(\"+", 0)
|> Js.log2("1 >")

// Part 2
module CSet = Set.Make({
  type t = char
  let compare = Pervasives.compare
})

Data.str |> AOC.str_split("\n\n") |> List.map(g => {
  let sets = g
  |> AOC.str_split("\n")
  |> List.map(AOC.charlist_of_string)
  //alt:  |> List.map(CSet.of_list)
  |> List.map(List.fold_left(CSet.add->AOC.frev, CSet.empty))

  sets |> List.fold_left(CSet.inter, List.hd(sets)) |> CSet.cardinal
}) |> List.fold_left(\"+", 0) |> Js.log2("2 >")
