module Data = Data_Day9
module AOC = AOC
AOC.print_header(9)

let preambleSize = 25
let codes = Data.str |> AOC.str_split("\n") |> List.map(Int64.of_string)

let preamble = codes |> AOC.take(preambleSize)
let body = codes |> AOC.drop(preambleSize)

type state = {
  preamble: list<Int64.t>,
  num: Int64.t,
}

// Part 1
let res = body |> List.fold_left((acc, num) => {
  if Int64.compare(acc.num, Int64.zero) > 0 {
    acc
  } else {
    let tuple = acc.preamble |> AOC.find_tuple((a, b) => Int64.add(a, b) |> Int64.compare(num) == 0)
    let preamble = List.tl(acc.preamble)->List.append(list{num})

    switch tuple {
    | Some(_, _) => {...acc, preamble: preamble}
    | None => {preamble: preamble, num: num}
    }
  }
}, {preamble: preamble, num: Int64.zero}) |> (a => a.num)
res->Int64.to_string->Js.log2("1 >", _)

// Part 2
type state2 = {
  seq: list<Int64.t>,
  success: bool,
}

codes
|> List.tl
|> List.fold_left((acc, e) => {
  acc.success
    ? acc
    : {
        let acc = {...acc, seq: List.append(acc.seq, list{e})}
        let trysum = acc.seq |> List.fold_left(Int64.add, Int64.zero)

        switch Int64.compare(res, trysum) {
        | 0 => {...acc, success: true}
        | cmp when cmp > 0 => acc
        | _ => {
            let seq = acc.seq |> List.fold_left((aa, _) => {
              let trysum = aa |> List.fold_left(Int64.add, Int64.zero)
              Int64.compare(res, trysum) >= 0 ? aa : List.tl(aa)
            }, acc.seq)
            let eq = seq |> List.fold_left(Int64.add, Int64.zero) |> Int64.compare(res)
            {seq: seq, success: eq == 0}
          }
        }
      }
}, {seq: list{List.hd(codes)}, success: false})
|> (
  a => {
    let _max = a.seq->AOC.max_of_list
    let _min = a.seq->AOC.min_of_list

    Int64.add(_max, _min)->Int64.to_string->Js.log2("2 >", _)
  }
)
