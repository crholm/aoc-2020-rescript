// https://adventofcode.com/2020/day/4

module Data = Data_Day4
module AOC = AOC
AOC.print_header(4)

/// Start: Map stuff
module StringMap = Map.Make({
  type t = string
  let compare = (a: string, b: string) => a == b ? 0 : a < b ? -1 : 1
})

type doc = StringMap.t<string>

let mget = (m, k) => {
  StringMap.mem(k, m) ? Some(StringMap.find(k, m)) : None
}

let int_in_range = (k, min, max, m) => {
  switch mget(m, k) {
  | Some(v) => {
      let i = int_of_string(v)
      min <= i && i <= max
    }
  | None => false
  }
}
let reg_match = (k, reg, m) => {
  switch mget(m, k) {
  | Some(v) =>
    switch Js.String2.match_(v, reg) {
    | Some(vv) => v == vv[0]
    | None => false
    }
  | None => false
  }
}
/// End: Map stuff

// Start: String stuff

let tuplesplit = (del, str) => {
  let l = AOC.str_split(del, str)
  (List.nth(l, 0), List.nth(l, 1))
}
let replace = (old, _new, str) => {
  Js.String2.replaceByRe(str, old, _new)
}
// End: String stuff

// Part 1

let passports =
  Data.str
  |> AOC.str_split("\n\n")
  |> List.map(s => s |> replace(%re("/\\n/g"), " "))
  |> List.map(s => {
    s |> AOC.str_split(" ") |> List.fold_left((acc, e) => {
      let (k, v) = e |> tuplesplit(":")
      StringMap.add(k, v, acc)
    }, StringMap.empty)
  })

passports |> List.filter(m => {
  switch StringMap.cardinal(m) {
  | 8 => true
  | 7 => !StringMap.mem("cid", m)
  | _ => false
  }
}) |> List.length |> Js.log2("1 >")

// Part 2

passports
// byr (Birth Year) - four digits; at least 1920 and at most 2002.
|> List.filter(int_in_range("byr", 1920, 2002))
// iyr (Issue Year) - four digits; at least 2010 and at most 2020.
|> List.filter(int_in_range("iyr", 2010, 2020))
// eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
|> List.filter(int_in_range("eyr", 2020, 2030))
// hgt (Height) - a number followed by either cm or in:
// If cm, the number must be at least 150 and at most 193.
// If in, the number must be at least 59 and at most 76.
|> List.filter(m => {
  !reg_match("hgt", %re("/(1|5|6|7)[0-9]{1,2}(cm|in)/"), m)
    ? false
    : {
        let v = StringMap.find("hgt", m)
        let prefix = v |> replace(%re("/(in|cm)/g"), "")
        let sufix = v |> replace(%re("/[0-9]/g"), "")
        let hgt = int_of_string(prefix)
        (sufix == "cm" && 150 <= hgt && hgt <= 193) || (sufix == "in" && 59 <= hgt && hgt <= 76)
      }
})
// hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
|> List.filter(reg_match("hcl", %re("/(#)[0-9a-f]{6}/")))
// ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
|> List.filter(reg_match("ecl", %re("/(amb|blu|brn|gry|grn|hzl|oth)/")))
// pid (Passport ID) - a nine-digit number, including leading zeroes.
|> List.filter(reg_match("pid", %re("/[0-9]{9}/")))
// cid (Country ID) - ignored, missing or not.
|> List.length
|> Js.log2("2 >")
