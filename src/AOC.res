let print_header = day => {
  Js.log3("\n=== AOC Day", day, "===")
}

let str_split = (del, str) => {
  Js.String2.split(str, del) |> Array.to_list
}

let str_join = (del: string, l: list<string>) => {
  l |> List.tl |> List.fold_left((acc, e) => {
    acc ++ del ++ e
  }, l |> List.hd)
}

let filteri: ((int, 'a) => bool, list<'a>) => list<'a> = (fn, l) => {
  let (_, res) = l |> List.fold_left((acc, e) => {
    let (i, l) = acc
    let r = fn(i, e) ? list{e} : list{}

    (i + 1, List.append(l, r))
  }, (0, list{}))
  res
}

let rec drop: (int, list<'a>) => list<'a> = (i, l) =>
  if l |> List.length < i {
    list{}
  } else if i > 0 {
    drop(i - 1, l |> List.tl)
  } else {
    l
  }

let take: (int, list<'a>) => list<'a> = (i, l) => {
  let rec take = (acc, l, i) => {
    i < 1 ? acc : take(List.append(acc, list{l |> List.hd}), l |> List.tl, i - 1)
  }

  if l |> List.length < i {
    list{}
  } else {
    take(list{}, l, i)
  }
}

let rec drop_while: (('a, list<'a>) => bool, list<'a>) => list<'a> = (fn, l) => {
  let h = l |> List.hd
  !fn(h, l) ? l : drop_while(fn, l |> List.tl)
}

let charlist_of_string = str => List.init(String.length(str), String.get(str))

let min_of_list = (l: list<'a>) => {
  List.fold_left(min, List.hd(l), l)
}

let max_of_list = (l: list<'a>) => {
  List.fold_left(max, List.hd(l), l)
}

let list_set = (i, el, l) => {
  l |> List.mapi((ii, e) => ii == i ? el : e)
}

let str_replace = (old, _new, str) => {
  Js.String2.replaceByRe(str, old, _new)
}

let frev: (('b, 'a) => 'c, 'a, 'b) => 'c = (fn, a, b) => fn(b, a)

let printn_l = (n, l) => {
  l |> filteri((i, _) => i < n) |> List.iteri((i, e) => {
    switch i {
    | 0 => Js.log2("{", e)
    | _ =>
      if i < n - 1 {
        Js.log2(",", e)
      } else {
        Js.log3(",", e, "}")
      }
    }
  })
}
let print_l = l => l |> printn_l(List.length(l))


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