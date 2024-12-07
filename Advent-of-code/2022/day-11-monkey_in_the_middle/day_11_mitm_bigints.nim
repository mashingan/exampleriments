from std/sugar import dump, `=>`, `->`
from std/sequtils import mapIt
from std/strutils import startsWith, parseInt, strip, splitWhitespace,
     contains, split
from std/deques import Deque, addLast, popFirst, len, initDeque, `$`
from std/algorithm import sort, SortOrder

import bigints

const
  input {.strdefine.} = "input.txt"
  minone = initBigInt(-1)
  zero = initBigInt(0)
  one = initBigInt 1

type
  Inspecting = proc(i: BigInt): BigInt
  Monkey = object
    items: Deque[BigInt]
    inspect: Inspecting
    divisibleBy: BigInt
    throwToIfDivisible: Natural
    throwToIfNotDivisible: Natural
    num: BigInt
    op: char

  Monkeys = seq[Monkey]

proc inspectThrow(monkeys: var Monkeys; total: var seq[Natural], reallyWorry = false) =
  for i, monkey in monkeys.mpairs:
    while monkey.items.len > 0:
      let worry = monkey.items.popFirst
      var newlevel = initBigInt 0
      if not reallyWorry:
        newlevel =  monkey.inspect(worry)
        newlevel = newlevel div 3.initBigInt
      else:
        newlevel = monkey.inspect(worry)
        #[
        if monkey.op == '+':
          if monkey.num == minone:
            newlevel = worry + worry
          else:
            newlevel = worry + monkey.num
          let rest = newlevel mod monkey.divisibleBy
          newlevel = (newlevel div monkey.divisibleBy) + rest
        elif monkey.op == '*':
          newlevel = (worry div monkey.divisibleBy) + (worry mod monkey.divisibleBy)
          if monkey.num == minone:
            newlevel *= newlevel
          else:
            newlevel *= monkey.num
      ]#
      if newlevel mod monkey.divisibleBy == zero:
        monkeys[monkey.throwToIfDivisible].items.addLast newlevel
      else:
        monkeys[monkey.throwToIfNotDivisible].items.addLast newlevel
      inc total[i]

proc main =
  var monkeys: Monkeys
  for line in input.lines:
    if line.len == 0:
      continue
    if line.startsWith "Monkey":
      monkeys.add Monkey(items: initDeque[BigInt]())
      continue
    if "Starting" in line:
      let nums = line.split(":")[1]
      for n in nums.split(","):
        monkeys[^1].items.addLast n.strip.parseInt.initBigInt
    elif "Operation" in line:
      let instr = line.split(":")[1].strip
      let eqtoken = instr.split("=").mapIt it.strip
      let rhs = eqtoken[1].splitWhitespace
      let num = if rhs[0] == "old" and rhs[2] == "old": -1
                elif rhs[0] == "old": parseInt rhs[2]
                else: parseInt rhs[0]
      monkeys[^1].num = initBigInt num
      monkeys[^1].op = rhs[1][0]
      closureScope:
        let num = initBigInt num
        case rhs[1]
        of "+":
          monkeys[^1].inspect = if num > minone: (n: BigInt) -> BigInt =>  n + num
                                else: (n: BigInt) -> BigInt => n + n
        of "*":
          monkeys[^1].inspect = if num > minone: (n: BigInt) -> BigInt =>  n * num
                                else: (n: BigInt) -> BigInt => (result = n * n)
        of "/":
          monkeys[^1].inspect = if num > minone: (n: BigInt) -> BigInt => n div num
                                else: (n: BigInt) -> BigInt => one
        of "-":
          monkeys[^1].inspect = if num > minone: (n: BigInt) -> BigInt => n - num
                                else: (n: BigInt) -> BigInt => zero
    elif "Test" in line:
      let divby = parseInt line.splitWhitespace()[^1]
      monkeys[^1].divisibleBy = initBigInt divby
    elif "true" in line:
      let to = parseInt line.splitWhitespace()[^1]
      monkeys[^1].throwToIfDivisible = to
    elif "false" in line:
      let to = parseInt line.splitWhitespace()[^1]
      monkeys[^1].throwToIfNotDivisible = to

  var
    inspectTotal = newSeq[Natural](monkeys.len)
    reallyWorryTotal = inspectTotal
    monkeysGroup2 = monkeys
    yes = true
  for _ in 1 .. 20:
    monkeys.inspectThrow inspectTotal
  for i in 1 .. 10_000:
    monkeysGroup2.inspectThrow reallyWorryTotal, reallyWorry = yes
    if i == 1: dump reallyWorryTotal
    elif i == 20: dump reallyWorryTotal
    elif i mod 1_000 == 0: dump reallyWorryTotal
  dump inspectTotal
  dump reallyWorryTotal
  sort inspectTotal, SortOrder.Descending
  sort reallyWorryTotal, SortOrder.Descending
  dump inspectTotal[0] * inspectTotal[1]
  dump reallyWorryTotal[0] * reallyWorryTotal[1]

main()
