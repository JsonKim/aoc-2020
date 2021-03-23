let input = Node.Fs.readFileAsUtf8Sync("src/aoc3/input.txt");

let s1 = (slopeRight, slopeDown) => {
  let fn = (acc, item, index) => {
    switch acc {
    | (right, nextLine, sum) when nextLine != index => (right, nextLine, sum)
    | (right, nextLine, sum) => (
        right + slopeRight,
        nextLine + slopeDown,
        sum +. if item->Js.String2.get(mod(right, item->Js.String2.length)) === "#" { 1. } else { 0. }
      )
    }
  }

  let (_, _, ret) = input->Js.String2.split("\n")->Belt.Array.reduceWithIndex((slopeRight, slopeDown, 0.), fn)
  ret
}

Js.log(s1(1, 1) *. s1(3, 1) *. s1(5, 1) *. s1(7, 1) *. s1(1, 2))

type x =
| Tree
| Empty

let encode = x => x->Js.String2.split("")->Belt.Array.map((x) => switch x {
  | "#" => Tree
  | _ => Empty
})

let input = input
  ->Js.String2.split("\n")
  ->Belt.Array.map(encode)

let makeCoords = ((slopeRight, slopeDown), width, height) => {
  let rec go = (x, y, result) =>
    if y > height-1 {
      result
    } else {
      list{(x, y), ...go(mod(x+slopeRight, width), y+slopeDown, result)}
    }

  go(slopeRight, slopeDown, list{})
}

let countTrees = (input, slope) =>
  slope
    ->makeCoords(input[0]->Js.Array2.length, input->Js.Array2.length)
    ->Belt.List.map(((x, y)) => input[y][x])
    ->Belt.List.keep(x => x == Tree)
    ->Belt.List.length

let countTreesWithInput = countTrees(input)

Js.log(countTreesWithInput((3, 1)))

Js.log(
  list{(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)}
    ->Belt.List.map(countTreesWithInput)
    ->Belt.List.reduce(1., (acc, x) => acc *. x->Belt.Int.toFloat)
)
