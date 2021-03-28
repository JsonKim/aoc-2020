open Belt

let input = Node.Fs.readFileAsUtf8Sync("src/aoc7/input.txt")

let input1 = `light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.`

let map2 = (a, b, f) => a->Option.flatMap(a => b->Option.map(b => f(a, b)))

let findA = s =>
  s->Js.String2.match_(%re("/^(.+) bags contain/"))

let findB = s =>
  s->Js.String2.match_(%re("/(([0-9]+) ([^.,]+)|no other) bags?[,.]/g"))

let containedBags = arr => arr
  ->Array.map(s => s
    ->Js.String2.match_(%re("/([0-9]+) (.*) bags?[,.]/"))
    ->Option.flatMap(arr => map2(arr[1], arr[2], (count, color) =>  {
      (count->Int.fromString->Option.getExn, color)
    }))
    ->Option.getWithDefault((0, "no other"))
  )

let parseLine = (line) => line
  ->((line) => (line->findA, line->findB))
  ->(((a, b)) => a->Option.flatMap(a => map2(a[0], b, (a, b) => 
    (a->Js.String2.replace(" bags contain", ""), b->containedBags)
  )))
  ->Option.getWithDefault(("no other", []))

let toDict = arr => arr->Array.reduce(Map.String.empty, (acc, (contained, colors)) =>
  colors->Array.reduce(acc, (acc, (_count, color)) =>
    acc->Map.String.update(color, (x) =>
      Some([contained]->Array.concat(x->Option.getWithDefault([]))))))

let rec traverse = (bags: Map.String.t<array<string>>, target: string, acc: Set.String.t) => bags
  ->Map.String.get(target)
  ->Option.mapWithDefault(acc, contained => contained->Array.reduce(acc, (acc, x) => {
    traverse(bags, x, acc->Set.String.add(x))
  }))

let parsed = input1
  ->Js.String2.split("\n")
  ->Array.map(parseLine)
  ->toDict

let part1 = parsed->traverse("shiny gold", Set.String.empty)->Set.String.size
Js.log(part1)

let toDict = arr => arr->Array.reduce(Map.String.empty, (acc, (contained, colors)) =>
  acc->Map.String.set(contained, colors)
)

let rec traverse = (bags: Map.String.t<array<(int, string)>>, target: string, init: int) => bags
  ->Map.String.get(target)
  ->Option.mapWithDefault(init, contain => contain->Array.reduceReverse(init, (acc, (count, color)) => {
    if color == "no other" {
      0
    } else {
      acc + count + count * (bags->traverse(color, init));
    }
  }))

let parsed = input
  ->Js.String2.split("\n")
  ->Array.map(parseLine)
  ->toDict

let part2 = parsed->traverse("shiny gold", 0)
Js.log(part2)

