// https://adventofcode.com/2020/day/5

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

Js.log("\n=== AOC Day 5 ===")

let bisect = (intr: interval, direction: char) => {
  // R means to take the upper half
  // B means to take the upper half
  // L means to take the lower half,
  // F means to take the lower half
  let move = (intr.max - intr.min) / 2
  switch direction {
  | 'B' | 'R' => {...intr, min: intr.min + move}
  | 'F' | 'L' => {...intr, max: intr.max - move}
  | _ => intr
  }
}

let sub_str_list = (str, start, len) => {
  let part = String.sub(str, start, len)
  List.init(String.length(part), String.get(part))
}

let toTicket = address => {
  let rowlist = sub_str_list(address, 0, 7)
  let collist = sub_str_list(address, 7, 3)

  let rowr = rowlist |> List.fold_left(bisect, {min: 0, max: 127})
  let colr = collist |> List.fold_left(bisect, {min: 0, max: 8})

  let row = List.hd(rowlist) == 'B' ? rowr.max : rowr.min
  let col = List.hd(rowlist) == 'L' ? colr.max : colr.min
  {row: row, col: col, num: row * 8 + col}
}

// Part 1
let tickets = Data_Day5.str |> split("\n") |> List.map(toTicket)

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
