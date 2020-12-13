open Belt

module AOC = AOC
module Data = Data_Day13
AOC.print_header(13)

let str = "939
7,13,x,x,59,x,31,19"

let tmp = Data.str->AOC.str_split("\n", _)
let offset = tmp->List.getExn(0)->int_of_string
let busses = tmp->List.getExn(1)->AOC.str_split(",", _)->List.keepMap(e => {
  switch e {
  | "x" => None
  | _ => Some(e->int_of_string)
  }
})

busses->List.map(n => {
  (n, n - mod(offset, n))
})->List.sort(((_, a), (_, b)) => a - b)->List.headExn->(((n, diff)) => n * diff)->Js.log2("1 >", _)

let str = "939
7,13,x,x,59,x,31,19" // 1068781
// let str2 = "123
// 1789,37,47,1889"
// let str = "939
// 7,13,59"

let tmp = Data.str->AOC.str_split("\n", _)
let busses = tmp->List.getExn(1)->AOC.str_split(",", _)->List.mapWithIndex((i, e) => {
  switch e {
  | "x" => None
  | _ => Some((e->int_of_string->Int64.of_int, i->Int64.of_int, e->int_of_string->Int64.of_int))
  }
})->List.keepMap(a => a)

// let busses = busses->List.sort(((_, a), (_, b)) => Int64.compare(a, b))

let rec calc = (mul, busses) => {
  let (n, _, scaler) = List.headExn(busses)
  let t = Int64.add(n, Int64.mul(scaler, mul))
  let res = busses->List.tailExn->List.every(((n, i, _)) => {
    Int64.rem(Int64.add(t, i), n)->Int64.compare(Int64.zero) == 0
  })
  if res {
    t
  } else {
    calc(Int64.add(mul, Int64.one), busses)
  }
}

let rec dcalc = (busses: list<(int64, int64, int64)>) => {
  //   Js.log("-->")
  //   busses->AOC.print_l
  if List.length(busses) == 1 {
    busses->List.headExn
  } else {
    let head = busses->List.take(2)->Option.getWithDefault(list{})
    let tail = busses->List.drop(2)->Option.getWithDefault(list{})
    let t = calc(Int64.zero, head)

    let (_, _, s1) = head->List.getExn(0)
    let (_, _, s2) = head->List.getExn(1)
    let scaler = Int64.mul(s1, s2)

    let n_buss = (t, Int64.zero, scaler)
    let busses = List.concat(list{n_buss}, tail)
    dcalc(busses)
  }
}

let res = dcalc(busses)
res->(((a, _, _)) => Int64.to_string(a))->Js.log2("2 >", _)
// === AOC Day 13 ===
// 1 > 4135
// 2 > 640856202464541
