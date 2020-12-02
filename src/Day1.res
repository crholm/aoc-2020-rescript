module Data = Day1_Data

let rec find_tuple = (target, l) => {
  switch l |> List.length {
  | 0 => None
  | _ => {
      let h = l |> List.hd
      let match = l |> List.tl |> List.filter(i => h + i == target)
      switch match |> List.length {
      | 0 => l |> List.tl |> find_tuple(target)
      | _ => Some(h, match |> List.hd)
      }
    }
  }
}

let rec find_thruple = (target, l) => {
  switch l |> List.length {
  | 0 => None
  | _ => {
      let z = l |> List.hd
      switch l |> List.tl |> find_tuple(target - z) {
      | None => l |> List.tl |> find_thruple(target)
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
      |> Js.log2(">" ++ xs ++ "+" ++ ys ++ " = " ++ string_of_int(x + y) ++ "\n")
  }
}

switch Data.list |> find_thruple(2020) {
| None => Js.log("No thruple match")
| Some(x, y, z) => {
    let xs = string_of_int(x)
    let ys = string_of_int(y)
    let zs = string_of_int(z)
    xs ++ "*" ++ ys ++ "*" ++ zs ++ " = " ++ string_of_int(x * y * z)
      |> Js.log2(">" ++ xs ++ "+" ++ ys ++ "+" ++ zs ++ " = " ++ string_of_int(x + y + z) ++ "\n")
  }
}
