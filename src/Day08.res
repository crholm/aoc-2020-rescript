// https://adventofcode.com/2020/day/8

module Data = Data_Day8
module AOC = AOC
AOC.print_header(8)

type instruction = NOP(int) | ACC(int) | JMP(int) | UNK
type register = {acc: int, trace: list<int>, success: bool}
let emptyState = {acc: 0, trace: list{}, success: false}

let program = Data.str |> AOC.str_split("\n") |> List.map(l => {
  let pair = l |> AOC.str_split(" ")
  let i = List.nth(pair, 1) |> int_of_string
  switch List.nth(pair, 0) {
  | "nop" => NOP(i)
  | "acc" => ACC(i)
  | "jmp" => JMP(i)
  | _ => UNK
  }
})

let rec exec = (pointer, state, program) => {
  if List.length(program) <= pointer {
    {...state, success: true}
  } else if pointer < 0 {
    {...state, success: false}
  } else if List.exists(i => i == pointer, state.trace) {
    {...state, success: false}
  } else {
    let state = {...state, trace: state.trace |> List.append(list{pointer})}
    switch List.nth(program, pointer) {
    | UNK => program |> exec(pointer + 1, state)
    | NOP(_) => program |> exec(pointer + 1, state)
    | ACC(i) => program |> exec(pointer + 1, {...state, acc: state.acc + i})
    | JMP(i) => program |> exec(pointer + i, state)
    }
  }
}

// Part 1

let state = program |> exec(0, emptyState)
state.acc |> Js.log2("1 >")

// Part 2
// Reducing over the previus trace, one by one, trying to
// flip one instruction at a time to see if the program terminates
// correctly
state.trace |> List.fold_left((cstate, i) => {
  let inst = List.nth(program, i)
  let inst = switch inst {
  | NOP(i) => JMP(i)
  | JMP(i) => NOP(i)
  | _ => inst
  }

  switch inst {
  | ACC(_) => cstate
  | _ when cstate.success => cstate
  | _ => {
      let state =
        program |> AOC.list_set(i, inst) |> exec(0, {acc: 0, trace: list{}, success: false})
      state.success ? state : cstate
    }
  }
}, emptyState) |> (s => s.acc) |> Js.log2("2 >")
