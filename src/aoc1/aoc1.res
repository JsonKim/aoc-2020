open Belt

let find2020 = (x: int, ys: list<int>, target) => ys
  ->List.getBy(y => (x + y) == target)
  ->Option.map(y => x * y)

let rec run = (xs: list<int>, target): option<int> =>
  xs->List.head->Option.flatMap(x =>
    xs->List.tail->Option.flatMap(ys =>
      switch find2020(x, ys, target) {
      | Some(s) => Some(s)
      | None => run(ys, target)
      }
    ))

let rec run2 = (xs: list<int>): option<int> =>
  xs->List.head->Option.flatMap(x =>
    xs->List.tail->Option.flatMap(ys =>
      switch ys->run(2020-x) {
      | Some(s) => Some(x * s)
      | None => run2(ys)
      }
    ))

let input = Node.Fs.readFileAsUtf8Sync("src/aoc1/input.txt")
let xs = input
  ->Js.String2.split("\n")
  ->Array.map(s => s->Int.fromString->Option.getExn)
  ->List.fromArray

Js.log(xs->run(2020))
Js.log(xs->run2)
