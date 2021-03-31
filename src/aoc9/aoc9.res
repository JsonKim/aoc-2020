open Belt

let find = (target, x, ys) => ys
  ->List.getBy(y => (x->Int.toFloat +. y->Int.toFloat) == target->Int.toFloat)
  ->Option.map(_ => target)

let rec run = (xs: list<int>, f): option<int> => {
  let f = (x, ys) =>
    switch f(x, ys) {
    | Some(s) => Some(s)
    | None => run(ys, f)
    }

  OptionM.lift2(f, xs->List.head, xs->List.tail)->OptionM.join
}

let rec attack = (xs: list<int>) => {
  let f = (x, ys) => 
    ys->List.take(25)->Option.flatMap(zs =>
      switch run(zs, find(x)) {
      | Some(_) => attack(ys)
      | None => Some(x)
      }
    )

  OptionM.lift2(f, xs->List.head, xs->List.tail)->OptionM.join
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
  } else {
    let s = s +. x
    let l = l->List.concat(list{x})

    if (s < weak) {
      (s, l)
    } else {
      let rec go = (s, l) => {
        OptionM.map2(l->List.head, l->List.tail, (h, t) => {
          let s = s -. h
          s > weak ? go(s, t) : (s, t)
        })
        ->Option.getWithDefault((s, l))
      }

      go(s, l)
    }
  }
})
->(((_, l)) => l->List.toArray)
->(l => l->Js.Math.minMany_float +. l->Js.Math.maxMany_float)

Js.log(xs->List.map(Int.toFloat)->f)
