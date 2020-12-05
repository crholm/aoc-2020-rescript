// https://adventofcode.com/2020/day/1
module Data = Data_Day1
module AOC = AOC
AOC.print_header(1)

let rec find: ('a => bool, list<'a>) => option<'a> = (fn, l) => {
  switch l |> List.length {
  | 0 => None
  | _ =>
    switch l |> List.hd |> fn {
    | true => Some(l |> List.hd)
    | false => find(fn, l |> List.tl)
    }
  }
}

let rec find_tuple: (('a, 'a) => bool, list<'a>) => option<('a, 'a)> = (fn, l) => {
  switch l |> List.length {
  | 0 => None
  | _ => {
      let x = l |> List.hd
      let tail = l |> List.tl

      switch tail |> find(x |> fn) {
      | None => tail |> find_tuple(fn)
      | Some(y) => Some(x, y)
      }
    }
  }
}

let rec find_thruple: (('a, 'a, 'a) => bool, list<'a>) => option<('a, 'a, 'a)> = (fn, l) => {
  switch l |> List.length {
  | 0 => None
  | _ => {
      let x = l |> List.hd
      let tail = l |> List.tl

      switch tail |> find_tuple(x |> fn) {
      | None => tail |> find_thruple(fn)
      | Some(y, z) => Some(x, y, z)
      }
    }
  }
}

// Part 1
let data = Data.str |> AOC.str_split("\n") |> List.map(int_of_string)

switch data |> find_tuple((x, y) => x + y == 2020) {
| None => Js.log("No tuple match")
| Some(x, y) => x * y |> Js.log2("1 >")
}

// Part 2
switch data |> find_thruple((x, y, z) => x + y + z == 2020) {
| None => Js.log("No thruple match")
| Some(x, y, z) => x * y * z |> Js.log2("2 >")
}
