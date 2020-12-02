module Data = Day1_Data

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

let rec find_tuple = (target, l) => {
  switch l |> List.length {
  | 0 => None
  | _ => {
      let x = l |> List.hd
      let tail = l |> List.tl
      switch tail |> find(y => x + y == target) {
      | None => tail |> find_tuple(target)
      | Some(y) => Some(x, y)
      }
    }
  }
}

let rec find_thruple = (target, l) => {
  switch l |> List.length {
  | 0 => None
  | _ => {
      let z = l |> List.hd
      let tail = l |> List.tl
      switch tail |> find_tuple(target - z) {
      | None => tail |> find_thruple(target)
      | Some(x, y) => Some(x, y, z)
      }
    }
  }
}

Js.log("=== AOC Day 1 ===")

switch Data.list |> find_tuple(2020) {
| None => Js.log("No tuple match")
| Some(x, y) => {
    let xs = string_of_int(x)
    let ys = string_of_int(y)
    xs ++ "*" ++ ys ++ " = " ++ string_of_int(x * y)
      |> Js.log2("> " ++ xs ++ "+" ++ ys ++ " = " ++ string_of_int(x + y) ++ "\n ")
  }
}

switch Data.list |> find_thruple(2020) {
| None => Js.log("No thruple match")
| Some(x, y, z) => {
    let xs = string_of_int(x)
    let ys = string_of_int(y)
    let zs = string_of_int(z)
    xs ++ "*" ++ ys ++ "*" ++ zs ++ " = " ++ string_of_int(x * y * z)
      |> Js.log2("> " ++ xs ++ "+" ++ ys ++ "+" ++ zs ++ " = " ++ string_of_int(x + y + z) ++ "\n ")
  }
}
