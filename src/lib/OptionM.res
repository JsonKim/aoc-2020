open Belt

include Option

let id = a => a

let unit = a => Some(a)

let map2 = (oa, ob, f) =>
  oa->flatMap(a =>
    ob->map(b =>
    f(a, b)))

let apply = (ma, mf) => switch mf {
| Some(f) => ma->map(f)
| None => None
}

let join = (mma) => mma->flatMap(id)

let lift2 = (f: ('a, 'b) => 'c) => (oa, ob) =>
  f->map(oa, _)->apply(ob, _)

let lift3 = (f: ('a, 'b, 'c) => 'd) => (oa, ob, oc) =>
  f |>map (oa) |>apply (ob) |>apply (oc)

let lift4 = (f: ('a, 'b, 'c, 'd) => 'e) => (oa, ob, oc, od) =>
  f->lift3(oa, ob, oc)->apply(od)
