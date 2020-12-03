module Data = Data_Day1

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

Js.log("=== AOC Day 1 ===")

switch Data.list |> find_tuple((x, y) => x + y == 2020) {
| None => Js.log("No tuple match")
| Some(x, y) => {
    let xs = string_of_int(x)
    let ys = string_of_int(y)
    xs ++ "*" ++ ys ++ " = " ++ string_of_int(x * y)
      |> Js.log2("1 > " ++ xs ++ "+" ++ ys ++ " = " ++ string_of_int(x + y) ++ "\n   ")
  }
}

switch Data.list |> find_thruple((x, y, z) => x + y + z == 2020) {
| None => Js.log("No thruple match")
| Some(x, y, z) => {
    let xs = string_of_int(x)
    let ys = string_of_int(y)
    let zs = string_of_int(z)
    xs ++ "*" ++ ys ++ "*" ++ zs ++ " = " ++ string_of_int(x * y * z)
      |> Js.log2(
        "2 > " ++ xs ++ "+" ++ ys ++ "+" ++ zs ++ " = " ++ string_of_int(x + y + z) ++ "\n   ",
      )
  }
}
