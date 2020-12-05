// https://adventofcode.com/2020/day/5

module Data = Data_Day5
module AOC = AOC
AOC.print_header(5)

type interval = {
  min: int,
  max: int,
}

type ticket = {
  row: int,
  col: int,
  num: int,
}

let bisect = (intr: interval, direction: char) => {
  // R means to take the upper half
  // B means to take the upper half
  // L means to take the lower half,
  // F means to take the lower half
  let move = (intr.max - intr.min) / 2 + 1
  switch direction {
  | 'B' | 'R' => {...intr, min: intr.min + move}
  | 'F' | 'L' => {...intr, max: intr.max - move}
  | _ => intr
  }
}

let to_ticket = address => {
  let row =
    address->String.sub(0, 7)
    |> AOC.charlist_of_string
    |> List.fold_left(bisect, {min: 0, max: 127})
    |> (i => i.max)
  let col =
    address->String.sub(7, 3)
    |> AOC.charlist_of_string
    |> List.fold_left(bisect, {min: 0, max: 7})
    |> (i => i.max)
  {row: row, col: col, num: row * 8 + col}
}

// Part 1
let tickets = Data_Day5.str |> AOC.str_split("\n") |> List.map(to_ticket)

tickets |> List.fold_left((max, t) => {
  t.num > max ? t.num : max
}, 0) |> Js.log2("1 >")

// Part 2
module ISet = Set.Make({
  type t = int
  let compare = (a: int, b: int) => a == b ? 0 : a < b ? -1 : 1
})

let imap = tickets |> List.fold_left((acc, t) => {
  ISet.add(t.num, acc)
}, ISet.empty)

tickets |> List.fold_left((acc, t) => {
  acc > 0 ? acc : !ISet.mem(t.num + 1, imap) && ISet.mem(t.num + 2, imap) ? t.num + 1 : 0
}, 0) |> Js.log2("2 >")

// Part 2, alt (differenc between sets)
let min = tickets |> List.map(e => e.num) |> AOC.min_of_list
let max = tickets |> List.map(e => e.num) |> AOC.max_of_list
let seg =
  List.init(max - min + 1, i => i + min) |> List.fold_left((acc, i) => ISet.add(i, acc), ISet.empty)

ISet.diff(seg, imap) |> ISet.elements |> List.hd |> Js.log2("2*>")
