// https://adventofcode.com/2020/day/5
Js.log("\n=== AOC Day 5 ===")

module Data = Data_Day5

let split = (del, str) => {
  Js.String2.split(str, del) |> Array.to_list
}

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

let make_list = str => {
  List.init(String.length(str), String.get(str))
}

let to_ticket = address => {
  let row =
    address->String.sub(0, 7)
    |> make_list
    |> List.fold_left(bisect, {min: 0, max: 127})
    |> (i => i.max)
  let col =
    address->String.sub(7, 3)
    |> make_list
    |> List.fold_left(bisect, {min: 0, max: 7})
    |> (i => i.max)
  {row: row, col: col, num: row * 8 + col}
}

// Part 1
let tickets = Data_Day5.str |> split("\n") |> List.map(to_ticket)

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
