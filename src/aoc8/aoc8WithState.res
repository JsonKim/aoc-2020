open Belt

type state<'s, 'a> = ('s) => ('a, 's)

let unit = (a): state<'s, 'a> => s => (a, s)

let flatMap = (sa: state<'s, 'a>, f: ('a) => state<'s, 'b>): state<'s, 'b> => s => {
  let (a, s1) = sa(s)
  f(a)(s1)
}

let map = (sa, f): state<'s, 'b> =>
  flatMap(sa, a => unit(f(a)))

let map2 = (sa, sb, f): state<'s, 'c> =>
  flatMap(sa, a =>
    map(sb, b =>
      f(a, b)))

let sequence = (fs: list<state<'s, 'a>>): state<'s, list<'a>> =>
  fs->List.reduce(unit(list{}), (sa, x) => map2(sa, x, (sa, x) => list{x, ...sa}))

let traverse = (f, la: list<state<'s, 'a>>): state<'s, list<'b>> =>
  sequence(la->List.map(f))

let get = (): state<'s, 's> => s => (s, s)

let set = (s): state<'s, unit> => _ => ((), s)

let modify = (f: ('s) => 's): state<'s, unit> =>
  flatMap(get(), s =>
    map(set(f(s)), _ =>
      ()))

type input = Acc(int) | Jmp(int) | Nop(int)

type machine = {
  len: int,
  pos: int,
  acc: int,
}

type result = (int, int)

let update = (input, s: machine) => {
  let next = x => mod(s.pos + x, s.len)

  switch input {
  | Acc(x) => { ...s, pos: next(1), acc: s.acc + x}
  | Jmp(x) => { ...s, pos: next(x) }
  | Nop(_) => { ...s, pos: next(1) }
  }
}

let simulateMachine = (inputs: list<input>): state<machine, list<machine>> => {
  let actions = inputs->List.map(i => s =>
    flatMap(get(), s =>
      map(modify(update(i)), _ =>
        s))(s))

  sequence(actions)
}

let instructions = list{
}

let s = instructions->simulateMachine({ len: instructions->List.length, pos: 0, acc: 0 })
