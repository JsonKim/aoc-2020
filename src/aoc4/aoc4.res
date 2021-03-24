open Belt

let input = Node.Fs.readFileAsUtf8Sync("src/aoc4/input.txt")

let id = x => x

let keys = list{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

let testData = `hcl:#6b5442 ecl:brn iyr:2019
pid:637485594 hgt:171cm
eyr:2021 byr:1986`

let validator = data => keys
  ->List.map(k => Js.Re.fromString("(^| |\\n)"++k++":")->Js.Re.test_(data))
  ->List.every(id)

let part1 = input
  ->Js.String2.split("\n\n")
  ->Array.map(validator)
  ->Array.keep(id)
  ->Array.length

Js.log(part1)
