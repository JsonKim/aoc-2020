let input = Node.Fs.readFileAsUtf8Sync("src/aoc5/input.txt");

let lines = input->Js.String2.split("\n")

let decode = (seat, one) => {
  let fn = ((exp, sum), item) =>
    (exp * 2, sum + (item === one ? exp : 0 ))

  let (_, sum) = seat->Js.String2.split("")->Belt.Array.reduceReverse((1, 0), fn)
  sum
}

let seatToNumber = (line) => {
  let seat = line->Js.String2.match_(%re("/(.{7})(.{3})/"))
  switch seat {
  | None => 0
  | Some(arr) => 
    arr[1]->decode("B") * 8 + arr[2]->decode("R")
  }
}

let max = lines
  ->Belt.Array.map(seatToNumber)
  ->Js.Math.maxMany_int

Js.log(max)

let sortedList = lines
  ->Belt.Array.map(seatToNumber)
  ->Belt.SortArray.stableSortBy((a, z) => a - z)
  ->Belt.List.fromArray

let mine = sortedList
  ->Belt.List.tailExn
  ->Belt.List.reduce((sortedList->Belt.List.headExn, list{}), ((prev, acc), curr) => {
    (curr, list{(prev, curr), ...acc})
  })
  ->((_, acc)) => acc
  ->Belt.List.keep(((prev, curr)) => (curr - prev) > 1)
  ->Belt.List.headExn
  ->((prev, _)) => prev + 1

Js.log(mine)
