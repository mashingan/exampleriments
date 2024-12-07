from std/sequtils import foldl
from std/sugar import dump
from std/sets import toHashSet, items, `*`, HashSet, pop

const input {.strdefine.} = "input.txt"

type
  LowerCase = range['a' .. 'z']
  UpperCase = range['A' .. 'Z']

func convert(c: char): int =
  if c in {'a' .. 'z'}:
    c.ord - LowerCase.low.ord + 1
  else:
    c.ord - UpperCase.low.ord +
    LowerCase.high.ord - LowerCase.low.ord + 2

proc main =
  var
    sumpriority = 0
    elfcount = 0
    sumbadge = 0
    elfRucksack = newseq[HashSet[char]](3)

  for line in input.lines:
    let
      mid = line.len div 2
      half1 = line[0 ..< mid]
      half2 = line[mid .. ^1]
    var itemType = half1.toHashSet * half2.toHashSet
    sumpriority += convert itemType.pop
    elfRucksack[elfcount mod elfRucksack.len] = line.toHashSet
    inc elfcount
    if elfcount mod elfRucksack.len == 0 and elfcount > 0:
      var theset = elfRucksack.foldl( a * b )
      let groupbadge = theset.pop
      sumbadge += convert groupbadge
  dump sumpriority
  dump sumbadge

main()
