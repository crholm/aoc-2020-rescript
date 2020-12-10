module Data = Data_Day10
module AOC = AOC
AOC.print_header(10)

let input = Data.str |> AOC.str_split("\n") |> List.map(int_of_string)

type diffs = {
  at: int,
  once: list<int>,
  twos: list<int>,
  threes: list<int>,
}
let empty = {at: 0, once: list{}, twos: list{}, threes: list{}}

let res = input |> List.sort(Pervasives.compare) |> List.fold_left((acc, e) => {
  let at = acc.at
  switch e - at {
  | 1 => {...acc, at: e, once: list{e, ...acc.once}}
  | 2 => {...acc, at: e, twos: list{e, ...acc.twos}}
  | 3 => {...acc, at: e, threes: list{e, ...acc.threes}}
  | _ => acc
  }
}, empty)

let max = input |> AOC.max_of_list

let res = {...res, threes: list{max + 3, ...res.threes}}

Js.log2("1 >", List.length(res.once) * List.length(res.threes))

let rec trace = (current, input) => {
  let len = List.length(input)
  if len == 0 {
    1
  } else if len > 2 && List.nth(input, 2) == current + 3 {
    trace(List.nth(input, 0), AOC.drop(1, input)) +
    trace(List.nth(input, 1), AOC.drop(2, input)) +
    trace(List.nth(input, 2), AOC.drop(3, input))
  } else if len > 1 && List.nth(input, 1) == current + 2 {
    trace(List.nth(input, 0), AOC.drop(1, input)) + trace(List.nth(input, 1), AOC.drop(2, input))
  } else {
    trace(List.nth(input, 0), AOC.drop(1, input))
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
  if e == acc.last + 3 {
    let part = acc.part |> List.sort(Pervasives.compare)
    let res = Int64.of_int(trace(List.hd(part), List.tl(part)))
    {part: list{e}, last: 0, res: Int64.mul(acc.res, res)}
  } else {
    {...acc, part: list{e, ...acc.part}, last: e}
  }
}, {part: list{}, last: 0, res: Int64.one})
|> (a => a.res)
|> Int64.to_string
|> Js.log2("2 >")
