open Belt

let input = Node.Fs.readFileAsUtf8Sync("src/aoc4/input.txt")

let id = a => a
let eq = (a, b) => a == b
let sequence = (xs) => xs->List.reduce(Some(list{}), (acc, x) => switch (acc, x) {
| (Some(acc), Some(a)) => Some(list{a, ...acc})
| _ => None
})

let mapTogether = (first: option<'a>,
                   second: option<'b>,
                   func: ('a, 'b) => 'c): option<'c>
  => Option.flatMap(first, f => Option.map(second, s => func(f, s)));

let getProp = (dict, prop) => Js.Dict.get(dict, prop)

let lift = (f, dict, prop) => getProp(dict, prop)->Option.flatMap(f)

let number = (dict, prop): option<float>
  => getProp(dict, prop)->Option.flatMap(json => Js.Json.decodeNumber(json))

let req = (t: option<'t>,
           prop: string,
           decode: ((Js.Dict.t<'a>, string) => option<'prop>),
           dict: Js.Dict.t<'a>,
           update: ('t, 'prop) => 't): option<'t>
  => mapTogether(t, decode(dict, prop), update)

let parseInt = (l, u, s) => s->Int.fromString->Option.flatMap(n => n >= l && n <= u ? Some(s) : None)
let parseWithRe = (re, s) => Js.Re.fromString(re)->Js.Re.test_(s) ? Some(s) : None

let parseBirthYear = lift(parseInt(1920, 2002))
let parseIssueYear = lift(parseInt(2010, 2020))
let parseExpirationYear = lift(parseInt(2020, 2030))

let parseHexColor = lift(parseWithRe("^#[0-9a-z]{6}$"))
let parseEyeColor = lift(parseWithRe("^(amb|blu|brn|gry|grn|hzl|oth)$"))
let parsePid = lift(parseWithRe("^[0-9]{9}$"))

let parseHeight = lift((s) => parseWithRe("^[0-9]{2,3}(cm|in)$", s)
  ->Option.flatMap(x =>
      %re("/cm$/")->Js.Re.test_(x)
        ? parseInt(150, 193, x)
        : parseInt(59, 76, x)
  ))

let keys = list{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

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
  ->Option.map(Js.Dict.fromList)

let data = input
  ->Js.String2.split("\n\n")

let const = (x, _) => x
let parse = (prop, decode, x) => Some(x)->req(prop, decode, x, const)

type passport = {
  byr: string,
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
}

let dictToPassport = x => ({
  byr: x->Js.Dict.unsafeGet("bry"),
  iyr: x->Js.Dict.unsafeGet("iyr"),
  eyr: x->Js.Dict.unsafeGet("eyr"),
  hgt: x->Js.Dict.unsafeGet("hgt"),
  hcl: x->Js.Dict.unsafeGet("hcl"),
  ecl: x->Js.Dict.unsafeGet("ecl"),
  pid: x->Js.Dict.unsafeGet("pid"),
})

let passportSchema1 = data
  =>parse("byr", parseBirthYear, data)
  ->Option.flatMap(parse("iyr", parseIssueYear))
  ->Option.flatMap(parse("eyr", parseExpirationYear))
  ->Option.flatMap(parse("hgt", parseHeight))
  ->Option.flatMap(parse("hcl", parseHexColor))
  ->Option.flatMap(parse("ecl", parseEyeColor))
  ->Option.flatMap(parse("pid", parsePid))
  ->Option.map(dictToPassport)

let passportParser = list{
  ("byr", parseBirthYear),
  ("iyr", parseIssueYear),
  ("eyr", parseExpirationYear),
  ("hgt", parseHeight),
  ("hcl", parseHexColor),
  ("ecl", parseEyeColor),
  ("pid", parsePid),
 }

let passportSchema2 = data => passportParser
  ->List.map(((key, f)) => parse(key, f, data))
  ->sequence
  ->Option.map(List.headExn)
  ->Option.map(dictToPassport)

let passportParser = schema => data => data
  ->getValues
  ->Option.flatMap(schema)

Js.log(data->Array.keepMap(passportParser(passportSchema1))->Array.length)
