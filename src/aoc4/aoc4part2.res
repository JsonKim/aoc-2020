open Belt

let input = Node.Fs.readFileAsUtf8Sync("src/aoc4/input.txt")

let id = a => a
let eq = (a, b) => a == b
let sequence = (xs) => xs->List.reduce(Some(list{}), (acc, x) => switch (acc, x) {
| (Some(acc), Some(a)) => Some(list{a, ...acc})
| _ => None
})

let parseInt = (l, u, s) => s->Int.fromString->Option.flatMap(n => n >= l && n <= u ? Some(s) : None)
let parseBirthYear = parseInt(1920, 2002)
let parseIssueYear = parseInt(2010, 2020)
let parseExpirationYear = parseInt(2020, 2030)

let parseWithRe = (re, s) => Js.Re.fromString(re)->Js.Re.test_(s) ? Some(s) : None
let parseHexColor = parseWithRe("^#[0-9a-z]{6}$")
let parseEyeColor = parseWithRe("^(amb|blu|brn|gry|grn|hzl|oth)$")
let parsePid = parseWithRe("^[0-9]{9}$")

let parseHeight = (s) => parseWithRe("^[0-9]{2,3}(cm|in)$", s)
  ->Option.flatMap(x => if %re("/cm$/")->Js.Re.test_(x) {
    parseInt(150, 193, x)
  } else {
    parseInt(59, 76, x)
  }
  )

Js.log("150cm"->parseHeight)

let keys = list{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

let parser = list{
  ("byr", parseBirthYear),
  ("iyr", parseIssueYear),
  ("eyr", parseExpirationYear),
  ("hgt", parseHeight),
  ("hcl", parseHexColor),
  ("ecl", parseEyeColor),
  ("pid", parsePid),
}

let findKeyRegEx = k => Js.Re.fromString("(^| |\\n)("++k++"):(.[^ \\n]*)")

let getValueByKey = (data, k) => data
  ->Js.String2.match_(k->findKeyRegEx)
  ->Option.flatMap((arr) => switch (arr[2], arr[3]) {
  | (Some(key), Some(value)) when k == key => Some((key, value))
  | _ => None
  })

let getValues = data => keys
  ->List.map(getValueByKey(data))
  ->sequence
  ->Option.flatMap(x => x->List.map(((key, value)) =>
      parser->List.getAssoc(key, eq)->Option.flatMap(f => f(value))
    )
    ->sequence)

let part2 = input
  ->Js.String2.split("\n\n")
  ->Array.map(getValues)
  ->Array.keep(x => x != None)
  ->Array.length

Js.log(part2)
