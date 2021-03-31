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
                        // do {
  flatMap(get(), s =>   //   s <- get
    map(set(f(s)), _ => //   _ <- set(f(s))
      ()))              //   pure ()
                        // }

type input = Acc(int) | Jmp(int) | Nop(int)
type inputs = array<input>

type exit = Loop | Terminate

type call = { pos: int, acc: int }

type machine = {
  inputs: inputs,
  // non-empty list
  call: call,
  run: list<call>,
}

type result = (int, exit)

let update = (action, last: call) => {
  switch action {
  | Acc(x) => { pos: last.pos + 1, acc: last.acc + x}
  | Jmp(x) => { pos: last.pos + x, acc: last.acc }
  | Nop(_) => { pos: last.pos + 1, acc: last.acc }
  }
}

let update = machine => {
  let last = machine.call
  let input = machine.inputs[last.pos]->Option.getExn
  let call = update(input, last)
  { ...machine, call, run: list{last, ...machine.run } }
}

let rec exec: state<machine, result> = s =>
  flatMap(get(), s => {
    let last = s.call
    let exit = s.inputs->Array.length == last.pos
    if exit {
      unit((last.acc, Terminate))
    } else if (s.run->List.has(last.pos, (call, pos) => call.pos == pos)) {
      unit((last.acc, Loop))
    } else {
      flatMap(modify(update), _ => exec)
    }
  })(s)

let change = (input: input): input => switch input {
| Acc(_) => input
| Jmp(x) => Nop(x)
| Nop(x) => Jmp(x)
}

let updateArray = (pos: int, f, inputs) => {
  inputs[pos]->Option.mapWithDefault(inputs, input => {
    let l = inputs->Array.slice(~offset=0, ~len=pos)
    let r = inputs->Array.sliceToEnd(pos+1)

    Array.concatMany([l, [f(input)], r])
  })
}

let backwardAndFix = ({ inputs, call, run }: machine): machine => {
  run->List.head->Option.flatMap(call =>
    run->List.tail->Option.map(run => {
      let inputs = updateArray(call.pos, change, inputs)
      { inputs, call, run }
    }))
    ->Option.getWithDefault({ inputs, call, run })
}

let parseLine = (s) => s
  ->Js.String2.match_(%re("/^(nop|acc|jmp) ([+-][0-9]+)$/"))
  ->Option.map((match) => {
    let value = match[2]->Option.getExn->Int.fromString->Option.getExn

    switch match {
    | [_, "acc", _] => Acc(value)
    | [_, "jmp", _] => Jmp(value)
    | [_, "nop", _] => Nop(value)
    | _ => Nop(0)
    }
  })
  ->Option.getExn

let input = Node.Fs.readFileAsUtf8Sync("src/aoc8/input.txt")
let instructions = input->Js.String2.split("\n")->Array.map(parseLine)

let (result, machine) = exec({ inputs: instructions, call: { pos: 0, acc: 0 }, run: list{} })

let rec fix = machine => {
  let x = backwardAndFix(machine)
  let ((acc, exit), _) = exec(x)
  exit == Terminate ? acc : fix(x)
}

let x = fix(machine)

Js.log(x)
