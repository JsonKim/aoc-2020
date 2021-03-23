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

let slidingWindow = (l, n) => {
  let rec go = (l, n, result) => {
    switch (l->Belt.List.take(n), l->Belt.List.tail) {
    | (Some(xs), Some(t)) => list{xs, ...go(t, n, result)}
    | _ => result
    }
  }

  go(l, n, list{})
}

let mine = sortedList
  ->slidingWindow(2)
  ->Belt.List.getBy((pair) =>
      switch pair {
      | list{x1, x2} => x2 - x1 > 1
      | _ => false
      })
  ->Belt.Option.flatMap(x => Belt.List.head(x))
  ->Belt.Option.map(x => x + 1)

Js.log(mine)
