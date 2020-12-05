// https://adventofcode.com/2020/day/3

module Data = Data_Day3
module AOC = AOC
AOC.print_header(3)

let filteri: ((int, 'a) => bool, list<'a>) => list<'a> = (fn, l) => {
  let (_, res) = l |> List.fold_left((acc, e) => {
    let (i, l) = acc
    let r = fn(i, e) ? list{e} : list{}

    (i + 1, List.append(l, r))
  }, (0, list{}))
  res
}

let trees = (dx, dy, world) => {
  world |> filteri((row, _) => {
    mod(row, dy) == 0
  }) |> List.mapi((i, str) => {
    let len = String.length(str)
    let ii = mod(dx * i, len)

    String.get(str, ii)
  }) |> List.filter(c => c == '#') |> List.length
}

let world = Data.str |> AOC.str_split("\n")

world |> trees(3, 1) |> Js.log2("1 >")

let slopes = list{(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)}

slopes |> List.fold_left((acc, slope) => {
  let (dx, dy) = slope
  let t = trees(dx, dy, world)

  t |> Int64.of_int |> Int64.mul(acc)
}, Int64.one) |> Int64.to_string |> Js.log2("2 >")
