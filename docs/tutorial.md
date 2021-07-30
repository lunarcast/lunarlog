# Tutorial

## Faq

### What is...

- a rule: a set of `to prove A must know B` propositions, also known in lunarlog as `branches`
- a branch: a `to prove A you must know B` proposition.
- a branch made out of: a `head` node (has a yellow-ish border) and any number of `body` nodes (no border)
- a head node: a head node described what a branch is trying to prove
- a non-head/body node: a body node is a prerequisite of proving the head-node. The solver uses these nodes to be able to solve constraints for yuo.

### What does it mean to...

- `create` a node: creating a node involves defining it's name and argument count.
- `use` a node: using a node means instantiating it in your current branch ()
- `create` a rule: creating a rule involves defining it's name and argument count.
- `create` a branch under rule: creating a branch defines a new way way to prove it/ parent rule

### Why...

- do numbers desugar to `S` and `Z` constructors: `Z` stands for zero, and `S` stands for successor. For more details, read about [chuch numerals](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals)
- do I get an infinite recursion error on my non-infinite query: the number of constrains required to solve a query grows exponentially with depth. To avoid lag spikes I decided to limit the maximum recursion depth to 100

### How to...

- show/hide the node edior: press `s`
- query your project: use the 5th panel in the gui view
- delete a node: press `delete` while holding the left mouse button down over the node
- move a node: use the left mouse button to drag it
- drag a node in/out of it's parent: use the left mouse button drag it in/out
- move the camera: use the left mouse button to drag outside a node
- connect 2 pins: click on the first pin, then click on the second
- delete a connections: click on it
- delete a branch: press the `delete` icon next to it
- select a branch: click on it

### Other

> Q: why do some nodes have a yellow-ish border? <br>
> A: a node having a yellow border is called a `head` node, and defines what the rules are trying to prove.

> Q: why does one of the brnches have a yellow border? <br>
> A: a branch has a yellow border if it's the branch you are currently editing. The node editor can be revealed by pressing `s`

## Query syntax:

- Constructors:

```
ConstructorName arg1 arg2 ... argN
```

where arg1, arg2... can be any expression

- Variables:

```
variableName
```

- Parenthesis

```
(...)
```

- Numbers

```haskell
0 -- desugars to `Z`
3 -- desugars to `S (S (S Z))`
```
