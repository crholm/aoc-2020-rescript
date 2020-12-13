open Belt

module AOC = AOC
module Data = Data_Day13
AOC.print_header(13)

type bus = {
  num: Int64.t,
  offset: Int64.t,
  scaler: Int64.t,
}

let earilest = Data.str->AOC.str_split("\n", _)->List.getExn(0)->Int64.of_string
let busses =
  Data.str
  ->AOC.str_split("\n", _)
  ->List.getExn(1)
  ->AOC.str_split(",", _)
  ->List.mapWithIndex((i, e) => {
    switch e {
    | "x" => None
    | _ =>
      Some({
        num: e->int_of_string->Int64.of_int,
        offset: i->Int64.of_int,
        scaler: e->int_of_string->Int64.of_int,
      })
    }
  })
  ->List.keepMap(a => a)

// Part 1

busses->List.map(n => {
  (n.num, Int64.sub(n.num, Int64.rem(earilest, n.num)))
})->List.sort(((_, a), (_, b)) =>
  Int64.compare(a, b)
)->List.headExn->(((n, diff)) => Int64.mul(n, diff))->Int64.to_string->Js.log2("1 >", _)

// Part 2
let rec common_time = (mul, t0: bus, t1: bus) => {
  let t = Int64.add(t0.num, Int64.mul(t0.scaler, mul))
  Int64.rem(Int64.add(t, t1.offset->Int64.sub(t0.offset)), t1.num)->Int64.compare(Int64.zero) == 0
    ? t
    : common_time(Int64.add(mul, Int64.one), t0, t1)
}

let rec calc = (busses: list<bus>) => {
  if List.length(busses) == 1 {
    let b = busses->List.headExn
    {...b, num: Int64.sub(b.num, b.offset), offset: Int64.zero}
  } else {
    let e0 = busses->List.getExn(0)
    let e1 = busses->List.getExn(1)
    let tail = busses->List.drop(2)->Option.getWithDefault(list{})

    let e0 = {...e0, num: common_time(Int64.one, e0, e1), scaler: Int64.mul(e0.scaler, e1.scaler)}
    calc(list{e0, ...tail})
  }
}

busses->calc->(a => Int64.to_string(a.num))->Js.log2("2 >", _)
