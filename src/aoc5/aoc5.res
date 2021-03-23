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
  ->Belt.Array.reduce(0, Js.Math.max_int)

Js.log(max)
