open Belt

module AOC = AOC
module Data = Data_Day11
AOC.print_header(11)

module GOL = {
  type tile = Floor | Empty | Occupied
  module XYCmp = Id.MakeComparable({
    type t = (int, int)
    let cmp = ((x0, y0), (x1, y1)) => {
      switch Pervasives.compare(x0, x1) {
      | 0 => Pervasives.compare(y0, y1)
      | c => c
      }
    }
  })
  let emptyMap: Map.t<XYCmp.t, tile, XYCmp.identity> = Map.make(~id=module(XYCmp))

  type t = {
    dim: (int, int), // dimx, dimy
    t: Map.t<XYCmp.t, tile, XYCmp.identity>,
  }
  let empty = {dim: (0, 0), t: emptyMap}
  let eq = (m1: t, m2: t) => {
    Map.eq(m1.t, m2.t, (v1, v2) => v1 == v2)
  }
  let get = (m: t, x, y) => {
    let t = Map.get(m.t, (x, y))
    switch t {
    | Some(t) => t
    | None => Floor
    }
  }

  let set_tile = (m: t, t: tile, x: int, y: int): t => {
    let mm = m.t->Map.set((x, y), t)
    {...m, t: mm}
  }

  let to_list = (m: t) => {
    m.t->Map.toList->List.map(((_, v: tile)) => v)
  }
  let to_string = (m: t) => {
    let (dimx, dimy) = m.dim
    let l = List.make(dimy, List.make(dimx, Floor))

    l->List.reduceWithIndex("", (acc, row, y) => {
      acc ++ row->List.reduceWithIndex("", (acc, _, x) => {
        acc ++
        switch get(m, x, y) {
        | Floor => "."
        | Empty => "L"
        | Occupied => "#"
        }
      }) ++ "\n"
    })
  }
  let print = (m: t) => to_string(m)->Js.log
}

type funs = {
  adjacent: (GOL.t, int, int) => list<GOL.tile>,
  map_tile: (GOL.t, int, int, (GOL.t, int, int) => list<GOL.tile>) => GOL.tile,
}

let update_state = (f: funs, m: GOL.t): GOL.t => {
  m.t->Map.toList->List.reduce(m, (acc, (k, _)) => {
    let (x, y) = k
    let n_tile = f.map_tile(m, x, y, f.adjacent)
    acc->GOL.set_tile(n_tile, x, y)
  })
}

let rec run = (current: GOL.t, last: GOL.t, fn: GOL.t => GOL.t) => {
  if GOL.eq(current, last) {
    current
  } else {
    let last = current
    let current = current->fn
    run(current, last, fn)
  }
}

let l = Data.str->AOC.str_split("\n", _)->List.map(line => {
  line->AOC.charlist_of_string->List.map(c => {
    switch c {
    | 'L' => GOL.Empty
    | '.' => GOL.Floor
    | x => {
        Js.log2("IT HAPPNED", x)
        GOL.Floor
      }
    }
  })
})

let initmap = l->List.reduceWithIndex(GOL.emptyMap, (acc, row, y) => {
  row->List.reduceWithIndex(acc, (acc, tile, x) => {
    Map.set(acc, (x, y), tile)
  })
})

// Part 1

let adjacentClose = (m: GOL.t, x, y) => {
  let (dimx, dimy) = m.dim
  list{(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)}
  ->List.map(((dx, dy)) => (dx + x, dy + y))
  ->List.keep(((x, y)) => {
    x > -1 && y > -1 && x < dimx && y < dimy
  })
  ->List.map(((x, y)) => m->GOL.get(x, y))
}

let map_tile4 = (m: GOL.t, x, y, adj): GOL.tile => {
  let tile = GOL.get(m, x, y)
  switch tile {
  | GOL.Floor => Floor
  | GOL.Empty => m->adj(x, y)->List.has(GOL.Occupied, (a, b) => a == b) ? GOL.Empty : GOL.Occupied
  | GOL.Occupied =>
    m->adj(x, y)->List.keep(t => t == GOL.Occupied)->List.length < 4 ? GOL.Occupied : GOL.Empty
  }
}

let pt1 = {adjacent: adjacentClose, map_tile: map_tile4}

let dimy = l->List.length
let dimx = l->List.headExn->List.length

let s1: GOL.t = {dim: (dimx, dimy), t: initmap}

let d = AOC.timer_start()
s1
->run(GOL.empty, update_state(pt1))
->GOL.to_list
->List.keep((t: GOL.tile) => t == Occupied)
->List.length
->Js.log2("1 >", _)

d->AOC.timer_stop("  >")
// Part 2

let adjacentFar = (m: GOL.t, x, y) => {
  let (dimx, dimy) = m.dim
  let _max = max(dimx, dimy)
  let directions = list{(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)}

  directions->List.reduce(list{}, (acc, (dx, dy)) => {
    let size = switch (dx, dy) {
    | (1, 0) => dimx - x
    | (-1, 0) => x
    | (0, 1) => dimy - y
    | (0, -1) => y
    | (1, 1) => max(dimx - x, dimy - y)
    | (-1, -1) => max(x, y)
    | (1, -1) => max(dimx - x, y)
    | (-1, 1) => max(x, dimy - y)
    | (_, _) => _max
    }

    let l = List.makeBy(size, i => i + 1)
    let maybeTile = l->List.reduce(None, (acc, i) => {
      switch acc {
      | Some(t) => Some(t)
      | None => {
          let x = dx * i + x
          let y = dy * i + y
          switch Map.get(m.t, (x, y)) {
          | Some(Floor) => None
          | Some(t) => Some(t)
          | None => Some(Floor)
          }
        }
      }
    })
    switch maybeTile {
    | Some(t) => list{t, ...acc}
    | None => acc
    }
  })->List.keep(a => a != Floor)
}

let map_tile5 = (m: GOL.t, x, y, adj): GOL.tile => {
  let tile = GOL.get(m, x, y)
  switch tile {
  | GOL.Floor => GOL.Floor
  | GOL.Empty => adj(m, x, y)->List.has(GOL.Occupied, (a, b) => a == b) ? GOL.Empty : GOL.Occupied
  | GOL.Occupied =>
    adj(m, x, y)->List.keep(t => t == GOL.Occupied)->List.length < 5 ? GOL.Occupied : GOL.Empty
  }
}

let pt2 = {adjacent: adjacentFar, map_tile: map_tile5}

let d = AOC.timer_start()
s1
->run(GOL.empty, update_state(pt2))
->GOL.to_list
->List.keep((t: GOL.tile) => t == GOL.Occupied)
->List.length
->Js.log2("2 >", _)

d->AOC.timer_stop("  >")
// 1 > 2448
// 2 > 2234
