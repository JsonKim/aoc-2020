open Belt

let input = Node.Fs.readFileAsUtf8Sync("src/aoc8/input.txt")

type instruction = Acc(int) | Jmp(int) | Nop(int)
type nextInst = Terminate | Pos(int)

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

let execute = (instructions, (pos, acc)) => {
  let len = instructions->Array.length
  let next = x =>
    (pos == len - 1)
      ? Terminate
      : Pos(mod(pos + x, len))

  instructions->Array.get(pos)->Option.map(instruction => switch instruction {
  | Acc(x) => (next(1), acc + x)
  | Jmp(x) => (next(x), acc)
  | Nop(_) => (next(1), acc)
  })
}

let program = (instructions) => {
  let rec go = (last, pos, acc) => {
    instructions
    ->execute((pos, acc))
    ->Option.mapWithDefault((Pos(pos), acc), (next) => switch next {
    | (Terminate, _) => next
    | (Pos(pos), acc) if last->Set.Int.has(pos) => (Pos(pos), acc)
    | (Pos(pos), acc) => go(last->Set.Int.add(pos), pos, acc)
    })
  }

  go(Set.Int.empty->Set.Int.add(0), 0, 0)
}

let instructions = input->Js.String2.split("\n")->Array.map(parseLine)
Js.log(instructions->program)
