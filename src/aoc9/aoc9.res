open Belt

let find = (x, ys, target) => ys
  ->List.getBy(y => (x->Int.toFloat +. y->Int.toFloat) == target->Int.toFloat)
  ->Option.map(_ => target)

let rec run = (xs: list<int>, target, f) =>
  xs->List.head->Option.flatMap(x =>
    xs->List.tail->Option.flatMap(ys =>
      switch f(x, ys, target) {
      | Some(s) => Some(s)
      | None => run(ys, target, f)
      }
    ))

let rec attack = (xs: list<int>) =>
  xs->List.head->Option.flatMap(x =>
    xs->List.tail->Option.flatMap(ys =>
      ys->List.take(25)->Option.flatMap(zs =>
        switch run(zs, x, find) {
        | Some(_) => attack(ys)
        | None => Some(x)
        }
      )))

let input = Node.Fs.readFileAsUtf8Sync("src/aoc9/input.txt")
let xs = input
  ->Js.String2.split("\n")
  ->Array.map(s => s->Int.fromString->Option.getExn)
  ->List.fromArray

let weak = xs->List.reverse->attack->Option.getExn->Int.toFloat

let f = (xs) => xs->List.reduce((0., list{}), ((s, l), x) => {
  if (s == weak) {
    (s, l)
  } else if (s > weak) {
    l->List.head->Option.flatMap(h =>
      l->List.tail->Option.map(t =>
        (s -. h->Int.toFloat, t)
      ))
      ->Option.getWithDefault((s, l))
  } else {
    (s +. x->Int.toFloat, l->List.concat(list{x}))
  }
})
->(((_, l)) => {
  let arr = l->List.toArray
  arr->Js.Math.maxMany_int->Int.toFloat +. arr->Js.Math.maxMany_int->Int.toFloat
})

Js.log(xs->f)
