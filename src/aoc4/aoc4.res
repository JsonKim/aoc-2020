open Belt

let input = Node.Fs.readFileAsUtf8Sync("src/aoc4/input.txt")

let id = x => x

let keys = list{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

let validator = data => keys
  ->List.map(k => Js.Re.fromString("(^| |\\n)"++k++":")->Js.Re.test_(data))
  ->List.every(id)

let part1 = input
  ->Js.String2.split("\n\n")
  ->Array.keep(validator)
  ->Array.length

Js.log(part1)
