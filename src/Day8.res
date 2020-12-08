module Data = Data_Day8
module AOC = AOC

let str = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
// 5

type instruction = NOP(int) | ACC(int) | JMP(int) | UNK
type register = {acc: int, trace: list<int>, success: bool}

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
  if state.trace |> List.exists(i => i == pointer) {
    state
  } else {
    let ins = List.nth(program, pointer)
    let trace = List.append(list{pointer}, state.trace)
    switch ins {
    | UNK | NOP(_) => exec(pointer + 1, {...state, trace: trace}, program)
    | ACC(i) => exec(pointer + 1, {...state, acc: state.acc + i, trace: trace}, program)
    | JMP(i) => exec(pointer + i, {...state, trace: trace}, program)
    }
  }
}

let state = program |> exec(0, {acc: 0, trace: list{}, success: false})
state.acc |> Js.log2("1 >")

// Part 2 ,, 8

let rec exec2 = (pointer, state, program) => {
  if pointer < 0 {
    {...state, success: false}
  } else if List.length(program) <= pointer {
    {...state, success: true}
  } else if state.trace |> List.exists(i => i == pointer) {
    {...state, success: false}
  } else {
    let ins = List.nth(program, pointer)
    let state = {...state, trace: state.trace |> List.append(list{pointer})}
    switch ins {
    | UNK | NOP(_) => exec2(pointer + 1, state, program)
    | ACC(i) => exec2(pointer + 1, {...state, acc: state.acc + i}, program)
    | JMP(i) => exec2(pointer + i, state, program)
    }
  }
}

state.trace |> List.fold_left((acc, i) => {
  if acc.success {
    acc
  } else {
    let inst = List.nth(program, i)

    let inst = switch inst {
    | NOP(i) => JMP(i)
    | JMP(i) => NOP(i)
    | _ => inst
    }

    switch inst {
    | ACC(_) => acc
    | _ => {
        let newp = program |> AOC.list_set(i, inst)

        let nstate = newp |> exec2(0, {acc: 0, trace: list{}, success: false})

        nstate.success ? nstate : acc
      }
    }
  }
}, {acc: 0, trace: list{}, success: false}) |> (s => s.acc) |> Js.log2("2 >")
