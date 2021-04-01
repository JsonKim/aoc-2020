open Belt

// xs를 head와 tail로 분리해서 실행해주는 함수
let applyHeadAndTail = (xs: list<'a>, f) => OptionM.lift2(f, xs->List.head, xs->List.tail)->OptionM.join

let findBy = f => (h, t) => t
  ->List.getBy(a => f(h, a))
  ->Option.map(a => (h, a))

let isTheSumOfTwoNumbersEqualTo = target => (a, b) => (a + b) == target

let liftedFind = target => xs => xs
  ->(target->isTheSumOfTwoNumbersEqualTo->findBy->applyHeadAndTail)
  ->Option.map(((x, y)) => x * y)

let rec scan = (xs: list<int>, f) => {
  let fn = (ys) =>
    switch f(ys) {
    | Some(s) => Some(s)
    | None => OptionM.lift2(scan, ys->List.tail, OptionM.unit(f))->OptionM.join
    }

  fn(xs)
}

let run = (xs: list<int>, target): option<int> => xs->scan(liftedFind(target))

let rec run2 = (xs: list<int>): option<int> =>
  xs->applyHeadAndTail((x, ys) =>
    switch ys->run(2020-x) {
    | Some(s) => Some(x * s)
    | None => run2(ys)
    }
  )

let input = Node.Fs.readFileAsUtf8Sync("src/aoc1/input.txt")
let xs = input
  ->Js.String2.split("\n")
  ->Array.map(s => s->Int.fromString->Option.getExn)
  ->List.fromArray

Js.log(xs->run(2020))
Js.log(xs->run2)
