// https://adventofcode.com/2020/day/2
Js.log("\n=== AOC Day 2 ===")

module Data = Data_Day2

type pass = {
  pwd: string,
  cha: char,
  min: int,
  max: int,
}

let split = (del, str) => {
  Js.String2.split(str, del) |> Array.to_list
}

let parse = raw => {
  let parts = raw |> split(" ")
  {
    pwd: parts->List.nth(2),
    cha: parts->List.nth(1)->String.get(0),
    min: parts->List.nth(0) |> split("-") |> List.hd |> int_of_string,
    max: parts->List.nth(0) |> split("-") |> List.rev |> List.hd |> int_of_string,
  }
}

Data.str |> split("\n") |> List.map(parse) |> List.map(p => {
  let l =
    List.init(String.length(p.pwd), String.get(p.pwd))
    |> List.filter(c => c == p.cha)
    |> List.length

  p.min <= l && l <= p.max
}) |> List.filter(a => a) |> List.length |> Js.log2("1 >")

Data.str |> split("\n") |> List.map(parse) |> List.map(p => {
  let l = List.init(String.length(p.pwd), String.get(p.pwd))
  let a = l->List.nth(p.min - 1) == p.cha
  let b = l->List.nth(p.max - 1) == p.cha
  (a && !b) || (!a && b) // xor
}) |> List.filter(a => a) |> List.length |> Js.log2("2 >")
