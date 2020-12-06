let print_header = day => {
  Js.log3("\n=== AOC Day", day, "===")
}

let str_split = (del, str) => {
  Js.String2.split(str, del) |> Array.to_list
}

let charlist_of_string = str => List.init(String.length(str), String.get(str))

let min_of_list = (l: list<'a>) => {
  List.fold_left(min, List.hd(l), l)
}

let max_of_list = (l: list<'a>) => {
  List.fold_left(max, List.hd(l), l)
}

let str_replace = (old, _new, str) => {
  Js.String2.replaceByRe(str, old, _new)
}

let frev: ('f, 'a, 'b) => 'c = (fn, a, b) => fn(b, a)
