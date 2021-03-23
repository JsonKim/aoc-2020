open Belt

let input = Node.Fs.readFileAsUtf8Sync("src/aoc-2017-6/input.txt");

type index = int
type block = int

type bank = (index, block)
type banks = list<bank>

let id = a => a
let inc = x => x + 1
let dec = x => x - 1
let eq = (a, b) => a == b
let pairMap = ((a, b), fn1, fn2) => (fn1(a), fn2(b))

let stringToInt = (s) => s->Int.fromString->Option.mapWithDefault(0, id)

let _banks: banks = input
  ->Js.String2.split("\t")
  ->Array.mapWithIndex((i, s) => (i, s->stringToInt))
  ->List.fromArray

list{(0, 1), (1, 2)}

let findMostBlock = (banks: banks) => banks->List.reduce((0, 0), (max, bank) => switch (max, bank) {
| ((_, block1), (_, block2)) when block2 > block1 => bank
| _ => max
})

let nextIndex = (pos, list) => mod(pos->inc, list->List.length)

let rec distribute = (banks, (pos, block)) => {
  if block <= 0 {
    banks
  } else {
    let value = banks->List.getAssoc(pos, eq)->Option.mapWithDefault(0, inc)
    let settedBanks = banks->List.setAssoc(pos, value, eq)
    settedBanks->distribute((pos->nextIndex(banks), block->dec))
  }
}

let redistribute = (banks) => {
  let (pos, block) = banks->findMostBlock
  let resetedBanks = banks->List.setAssoc(pos, 0, eq)
  resetedBanks->distribute((pos->nextIndex(banks), block))
}

let rec processing = (banks, prev) => {
  let banks = banks->redistribute
  Js.log(prev->List.length)
  switch prev->List.getBy(x => x == banks) {
  | Some(_) => list{banks, ...prev}
  | _ => processing(banks, list{banks, ...prev})
  }
}

let p1 = list{(0,0),(1,2),(2,7),(3,0)}->processing(list{})
let p1 = _banks->processing(list{})

Js.log(p1->List.length)
