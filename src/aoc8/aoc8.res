open Belt

let input = Node.Fs.readFileAsUtf8Sync("src/aoc8/input.txt")

type instruction = Acc(int) | Jmp (int) | Nop(int)
type instructions = array<instruction>

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
  let next = x => mod((pos + x), instructions->Array.length)

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
    ->Option.mapWithDefault(acc, (next) => switch next {
    | (pos, _) if last->Set.Int.has(pos) => acc
    | (pos, acc) => go(last->Set.Int.add(pos), pos, acc)
    })
  }

  go(Set.Int.empty->Set.Int.add(0), 0, 0)
}

let instructions = input->Js.String2.split("\n")->Array.map(parseLine)
Js.log(instructions->program)
