from std/sugar import dump
from std/decls import byaddr
from std/sequtils import repeat
from std/strutils import splitWhitespace, parseInt

const input {.strdefine.} = "input.txt"

type
  Pos = (int, int)
  Rope = object
    knots: seq[Pos]
    trails: seq[Pos]

  Dir = enum
    Down = 'D'
    Left = 'L'
    Right = 'R'
    Up = 'U'

func move(prev: var Pos, next: Pos) =
  prev[0] += next[0]
  prev[1] += next[1]

func adjacent(pos, target: Pos): bool =
  for i in -1 .. 1:
    for j in -1 .. 1:
      if (pos[0]+i, pos[1]+j) == target:
        return true
  false

func distanceTo(p1, p2: Pos): Pos =
  result = (p2[0]-p1[0], p2[1]-p1[1])
  result[0] = cmp(result[0], 0)
  result[1] = cmp(result[1], 0)

func alignedWith(p1, p2: Pos): bool =
  p1[0] == p2[0] or p1[1] == p2[1]

func move(rope: var Rope, dir: Dir, n: int) =
  let step = case dir
    of Down: (0, -1)
    of Left: (-1, 0)
    of Right: (1, 0)
    of Up: (0, 1)

  for i in 1 .. n:
    rope.knots[0].move step
    for j in rope.knots.low+1 .. rope.knots.high:
      var head {.byaddr.} = rope.knots[j-1]
      var tail {.byaddr.} = rope.knots[j]
      if tail.adjacent head: continue
      let dist = tail.distanceTo head
      tail.move dist
      if not tail.alignedWith head:
        if dist[0] == 0:
          tail[0] = head[0]
        elif dist[1] == 0:
          tail[1] = head[1]
    if rope.knots[^1] notin rope.trails:
      rope.trails.add rope.knots[^1]

proc main =
  var rope1 = Rope(
    knots: (0, 0).repeat 2,
    trails: @[(0, 0)],
  )
  var rope2 = Rope(
    knots: (0, 0).repeat 10,
    trails: @[(0, 0)],
  )
  for line in input.lines:
    let inst = line.splitWhitespace(maxsplit = 2)
    let dir = inst[0][0].Dir
    let n = parseInt inst[1]
    rope1.move dir, n
    rope2.move dir, n
  dump rope1.trails.len
  dump rope2.trails.len

main()
