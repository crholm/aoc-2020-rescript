open Belt

module AOC = AOC
module Data = Data_Day12
AOC.print_header(12)

let to_manhattan = (x, y) => {
  abs(x) + abs(y)
}

type direction = North | East | South | West
let delta = (dir, scaler) => {
  switch dir {
  | North => (0, 1 * scaler)
  | East => (1 * scaler, 0)
  | South => (0, -1 * scaler)
  | West => (-1 * scaler, 0)
  }
}

let to_direction = dir => {
  switch dir {
  | (0, 1) => North
  | (1, 0) => East
  | (0, -1) => South
  | (-1, 0) => West
  | _ => East
  }
}
let of_direction = dir => delta(dir, 1)

let apply_turn_left = ((dx, dy), deg) => {
  switch deg {
  | 90 => (-dy, dx)
  | 180 => (-dx, -dy)
  | 270 => (dy, -dx)
  | _ => (dx, dy)
  }
}

let turn_left = (current, deg) => {
  let xy = current->of_direction
  apply_turn_left(xy, deg)->to_direction
}

type instruction =
  North(int) | East(int) | South(int) | West(int) | Left(int) | Right(int) | Forward(int)

let prog = Data.str->AOC.str_split("\n", _)->List.keepMap(l => {
  let dir = l->String.get(0)
  let num = l->String.sub(1, String.length(l) - 1)->int_of_string
  switch dir {
  | 'N' => Some(North(num))
  | 'E' => Some(East(num))
  | 'S' => Some(South(num))
  | 'W' => Some(West(num))
  | 'L' => Some(Left(num))
  | 'R' => Some(Right(num))
  | 'F' => Some(Forward(num))
  | _ => None
  }
})

type state = {
  dir: direction,
  x: int,
  y: int,
}
let empty = {dir: East, x: 0, y: 0}

let state = prog->List.reduce(empty, (acc, ins) => {
  switch ins {
  | North(i) => {...acc, y: acc.y + i}
  | East(i) => {...acc, x: acc.x + i}
  | South(i) => {...acc, y: acc.y - i}
  | West(i) => {...acc, x: acc.x - i}
  | Forward(scaler) => {
      let (xx, yy) = acc.dir->delta(scaler)
      {...acc, x: acc.x + xx, y: acc.y + yy}
    }
  | Left(i) => {
      let n_dir = acc.dir->turn_left(i)
      {...acc, dir: n_dir}
    }
  | Right(i) => {
      let n_dir = acc.dir->turn_left(360 - i)
      {...acc, dir: n_dir}
    }
  }
})

state->(a => to_manhattan(a.x, a.y))->Js.log2("1 >", _)

type wstate = {
  x: int,
  y: int,
  wx: int,
  wy: int,
}

let empty2 = {x: 0, y: 0, wx: 10, wy: 1}

prog->List.reduce(empty2, (acc, ins) => {
  switch ins {
  | North(i) => {...acc, wy: acc.wy + i}
  | East(i) => {...acc, wx: acc.wx + i}
  | South(i) => {...acc, wy: acc.wy - i}
  | West(i) => {...acc, wx: acc.wx - i}
  | Forward(scaler) => {...acc, x: acc.x + acc.wx * scaler, y: acc.y + acc.wy * scaler}
  | Left(i) => {
      let (wx, wy) = apply_turn_left((acc.wx, acc.wy), i)
      {...acc, wx: wx, wy: wy}
    }
  | Right(i) => {
      let (wx, wy) = apply_turn_left((acc.wx, acc.wy), 360 - i)
      {...acc, wx: wx, wy: wy}
    }
  }
})->(a => to_manhattan(a.x, a.y))->Js.log2("2 >", _)
