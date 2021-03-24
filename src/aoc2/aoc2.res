open Belt

let input = Node.Fs.readFileAsUtf8Sync("src/aoc2/input.txt");

let line = "17-19 p: pwpzpfbrcpppjppbmppp"

let id = a => a

type passwordWithPolicy = {
  lowest: int,
  highest: int,
  letter: string,
  password: string,
}


let toInt = s => s->Option.mapWithDefault(0, s => s->Int.fromString->Option.getWithDefault(0))

let deserialize = line =>
  line
    ->Js.String2.match_(%re("/^([0-9]+)-([0-9]+) ([a-z]): (.*)$/"))
    ->Option.mapWithDefault({ lowest: 0, highest: 0, letter: " ", password: "" }, (x) => {
      lowest: x[1]->toInt,
      highest: x[2]->toInt,
      letter: x[3]->Option.getWithDefault(" "),
      password: x[4]->Option.getWithDefault(""),
    })

let count = (input, validator) => input
  ->Js.String2.split("\n")
  ->Array.map(deserialize)
  ->Array.map(validator)
  ->Array.keep(id)
  ->Array.length

let isValidForPart1 = (p) => {
  let letters = p.password->Js.String2.split("")->Array.keep(x => x == p.letter)
  let len = letters->Array.length
  len >= p.lowest && len <= p.highest
}

let part1 = input->count(isValidForPart1)

Js.log(part1)

let isValidForPart2 = (p) => {
  let (l, h) = (p.password->Js.String2.get(p.lowest-1), p.password->Js.String2.get(p.highest-1))
  l != h && (l == p.letter || h == p.letter)
}

let part2 = input->count(isValidForPart2)
  
Js.log(part2)
