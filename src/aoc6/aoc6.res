open Belt

let input = Node.Fs.readFileAsUtf8Sync("src/aoc6/input.txt");

let total = input
  ->Js.String2.split("\n\n")
  ->Array.map(s => s->Js.String2.replaceByRe(%re("/\\n/g"), "")->Js.String2.split(""))
  ->Array.map(Belt.Set.String.fromArray)
  ->Array.map(Belt.Set.String.size)
  ->Array.reduce(0, (acc, x) => acc + x)

Js.log(total)

let stringToSet = (s) => s->Js.String2.split("")->Set.String.fromArray
let allAnswered = "abcdefghijklmnopqrstuvwxyz"->stringToSet
let everyoneAnswerd = s => s
  ->Array.map(stringToSet)
  ->Array.reduce(allAnswered, Set.String.intersect)
  ->Set.String.size

let total2 = input
  ->Js.String2.split("\n\n")
  ->Array.map(s => s->Js.String2.split("\n"))
  ->Array.map(everyoneAnswerd)
  ->Array.reduce(0, (acc, x) => acc + x)

Js.log(total2)
 