from std/sequtils import toSeq, map, mapIt
from std/strutils import split, parseInt
from std/sugar import dump
const input {.strdefine.} = "input.txt"

proc main =
  var
    overlap = 0
    rangeOverlap = 0
  for line in input.lines:
    let
      pairs = line.split(",").mapIt(it.split("-").map parseInt)
      elf1 = { pairs[0][0].uint8 .. pairs[0][1].uint8 }
      elf2 = { pairs[1][0].uint8 .. pairs[1][1].uint8 }
    if elf1 <= elf2 or elf2 <= elf1:
      inc overlap
    if (elf1 * elf2).len > 0:
      inc rangeOverlap
  dump overlap
  dump rangeOverlap

main()
