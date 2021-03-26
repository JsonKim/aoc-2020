open Belt

let input = Node.Fs.readFileAsUtf8Sync("src/aoc7/input.txt")

type rec tree<'a> = {
  value: 'a,
  forest: list<tree<'a>>
}

let make = (value, forest) => { value, forest }

let append = ({ value, forest }, tree) => make(value, list{ tree, ...forest })

let rec elem = (fa, a) =>
  a == fa.value
  ? true
  : fa.forest->List.some((tree) => tree->elem(a))

let rec find = (fa, a) =>
  a == fa.value
  ? Some(fa)
  : fa.forest->List.reduce(None, (acc, tree) => acc != None ? acc : tree->find(a))

let rec draw = (forest, indentation) => {
  let len = forest->List.length

  forest->List.reduceWithIndex("", (r, tree, i) => {
    let isLast = i === len - 1
    r ++ indentation ++ (isLast ? `└` : `├`) ++ `─ ` ++ tree.value
      ++ tree.forest->draw(indentation ++ (len > 1 && !isLast ? `│  ` : `   `))
   })
}

let drawForest = (forest) => forest->draw("\n")

let drawTree = (tree) => tree.value ++ drawForest(tree.forest)

let rec map = (fa, f) => {
  value: f(fa.value),
  forest: fa.forest->List.map(tree => tree->map(f))
}

let fold = (tree, f) => {
  let rec go = (tree) => f(tree.value, tree.forest->List.map(go))
  go(tree)
}

let rec reduce = (fa, b, f) =>
  fa.forest->List.reduce(f(b, fa.value), (acc, tree) => tree->reduce(acc, f))

let t = {
  value: "1",
  forest: list{
    {
      value: "2",
      forest: list{
        {
          value: "3",
          forest: list{
            { value: "4", forest: list{} },
            { value: "5", forest: list{} },
            { value: "6", forest: list{} },
            { value: "7", forest: list{} },
          }
        }
      }
    },
    { value: "22", forest: list{} }
  }
}

/*
let root = make("empty", list{})

Js.log(root
  ->insert("bright white", "light red")
  ->insert("muted yellow", "light red")
  ->insert("bright white", "dark orange")
  ->insert("muted yellow", "dark orange")
  ->insert("shiny gold", "bright white")
  ->insert("shiny gold", "muted yellow")
  ->drawTree)
*/
Js.log(t->find("2"))