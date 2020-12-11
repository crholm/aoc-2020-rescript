module Data = Data_Day10
module AOC = AOC
AOC.print_header(10)

let input = Data.str |> AOC.str_split("\n") |> List.map(int_of_string)
let max = input->AOC.max_of_list
let input = list{0, max + 3, ...input}

type diffs = {
  at: int,
  once: int,
  threes: int,
}
let empty = {at: 0, once: 0, threes: 0}

input |> List.sort(Pervasives.compare) |> List.fold_left((acc, e) => {
  let at = acc.at
  switch e - at {
  | 1 => {...acc, at: e, once: acc.once + 1}
  | 3 => {...acc, at: e, threes: acc.threes + 1}
  | _ => acc
  }
}, empty) |> (a => a.once * a.threes |> Js.log2("1 >"))

let rec trace = (current, input) => {
  let len = List.length(input)
  switch len {
  | 0 => 1
  | l when l > 2 && List.nth(input, 2) == current + 3 =>
    trace(List.nth(input, 0), AOC.drop(1, input)) +
    trace(List.nth(input, 1), AOC.drop(2, input)) +
    trace(List.nth(input, 2), AOC.drop(3, input))
  | l when l > 1 && List.nth(input, 1) == current + 2 =>
    trace(List.nth(input, 0), AOC.drop(1, input)) + trace(List.nth(input, 1), AOC.drop(2, input))
  | _ => trace(List.nth(input, 0), AOC.drop(1, input))
  }
}

type partition = {
  part: list<int>,
  last: int,
  res: Int64.t,
}

input
|> List.append(list{0, max + 3})
|> List.sort(Pervasives.compare)
|> List.fold_left((acc, e) => {
  e != acc.last + 3
    ? {...acc, part: list{e, ...acc.part}, last: e}
    : {
        let part = acc.part |> List.sort(Pervasives.compare)
        let res = Int64.of_int(trace(List.hd(part), List.tl(part)))
        {part: list{e}, last: 0, res: Int64.mul(acc.res, res)}
      }
}, {part: list{}, last: 0, res: Int64.one})
|> (a => a.res)
|> Int64.to_string
|> Js.log2("2 >")
