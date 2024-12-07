from std/sugar import dump
from std/deques import Deque, toDeque, popLast, addFirst, addLast, len
from std/strutils import isDigit
from std/strscans import scanf

const input {.strdefine.} = "input.txt"

type
  CrateMover = enum
    Mover9000
    Mover9001

func stackConfig(line: string): seq[char] =
  result = newseq[char]((line.len+1) div 4)
  var pos = 0
  for r in result.mitems:
    let bound = min(pos+2, line.len-1)
    r = line[pos .. bound][1]
    pos += 4

func apply(stacks: var seq[Deque[char]], instruction: string, mover = Mover9000) =
  const format = "move $i from $i to $i"
  var
    toMove, fromStack, toStack: int
  discard scanf(instruction, format, toMove, fromStack, toStack)
  if mover == Mover9000:
    for _ in 1 .. toMove:
      stacks[toStack-1].addLast stacks[fromStack-1].popLast
  else:
    var tempstack = newseq[char](toMove)
    for temp in tempstack.mitems:
      temp = stacks[fromStack-1].popLast
    for i in countdown(tempstack.high, tempstack.low):
      stacks[toStack-1].addLast tempstack[i]

func reconfigure(cfg: sink seq[seq[char]]): unown seq[Deque[char]] =
  if cfg.len == 0: return
  result = newseq[Deque[char]](cfg[0].len)
  for row in cfg:
    for i, col in row:
      if col == ' ' or col.isDigit: continue
      result[i].addFirst col

proc main =
  var
    readconfig = true
    stacks: seq[Deque[char]]
    stacks2: seq[Deque[char]]
    cfg: seq[seq[char]]
  for line in input.lines:
    if line.len == 0:
      readconfig = false
      stacks = reconfigure cfg
      stacks2 = stacks
      continue

    if readconfig:
      cfg.add stackConfig(line)
      continue

    stacks.apply line
    stacks2.apply line, Mover9001

  var
    topMover9000 = ""
    topMover9001 = ""
  for i in stacks.low .. stacks.high:
    var s1 = stacks[i]
    var s2 = stacks2[i]
    if s1.len > 0: topMover9000 &= s1.popLast
    if s2.len > 0: topMover9001 &= s2.popLast

  dump topMover9000
  dump topMover9001

main()
