open Belt

let map2 = (oa, ob, f) =>
  oa->Option.flatMap(a =>
    ob->Option.map(b =>
      f(a, b)))

let apply = (mf, ma) => switch mf {
| Some(f) => ma->Option.map(f)
| None => None
}

let lift2 = (f: ('a, 'b) => 'c) => (oa, ob) =>
  oa->Option.map(f)->apply(ob)

let lift3 = (f: ('a, 'b, 'c) => 'd) => (oa, ob, oc) =>
  oa->Option.map(f)->apply(ob)->apply(oc)

let lift4 = (f: ('a, 'b, 'c, 'd) => 'e) => (oa, ob, oc, od) =>
  f->lift3(oa, ob, oc)->apply(od)

let find = (x, ys, target) => ys
  ->List.getBy(y => (x->Int.toFloat +. y->Int.toFloat) == target->Int.toFloat)
  ->Option.map(_ => target)

let rec run = (xs: list<int>, target, f): option<int> => {
  let f = (x, ys) =>
      switch f(x, ys, target) {
      | Some(s) => Some(s)
      | None => run(ys, target, f)
      }

  lift2(f, xs->List.head, xs->List.tail)->Option.flatMap(x => x)
}

let rec attack = (xs: list<int>) => {
  let f = (x, ys) => 
      ys->List.take(25)->Option.flatMap(zs =>
        switch run(zs, x, find) {
        | Some(_) => attack(ys)
        | None => Some(x)
        }
    )

  lift2(f, xs->List.head, xs->List.tail)->Option.flatMap(x => x)
}

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
