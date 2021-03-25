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

let parse = line =>
  line
    ->Js.String2.match_(%re("/^([0-9]+)-([0-9]+) ([a-z]): (.*)$/"))
    ->Option.flatMap((x) => switch x {
    | [_, l, h, letter, password] => Some({
      lowest: l->Int.fromString->Option.getExn,
      highest: h->Int.fromString->Option.getExn,
      letter,
      password,
    })
    | _ => None
   })

let validate = (arr, validator) => arr
  ->Array.keep(validator)
  ->Array.length

let count = (input, validator) => input
  ->Js.String2.split("\n")
  ->Array.keepMap(parse)
  ->validate(validator)

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
