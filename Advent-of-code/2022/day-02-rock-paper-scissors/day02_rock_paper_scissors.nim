from std/strutils import parseEnum

const input {.strdefine.} = "input.txt"

type
  ResponseMove = enum
    Rock = (1, "X")
    Paper = (2, "Y")
    Scissors = (3, "Z")

  FirstMove = range['A' .. 'C']

const
  moves: array[FirstMove, ResponseMove] = [Rock, Paper, Scissors]
  winScore = 6
  drawScore = 3
  loseScore = 0

func scorePart1(move: FirstMove, resp: ResponseMove): int =
  result = resp.int
  let idx = FirstMove.low.ord + (result-1) - move.ord
  if idx == 0: result += drawScore
  elif idx == 1 or idx == -2: result += winScore
  else: discard

func scorePart2(move: FirstMove, resp: ResponseMove): int =
  if resp == Paper:
    result = drawScore + moves[move].int
  elif resp == Rock:
    let hand = move.ord - FirstMove.low.ord - 1
    let handmove = hand + 'A'.ord
    result = loseScore + (if hand == -1: ResponseMove.high.int else: moves[FirstMove handmove].int)
  else:
    let hand = move.ord - FirstMove.low.ord + 1
    let handmove = hand + 'A'.ord
    result = winScore + (if hand >= moves.len: ResponseMove.low.int else: moves[FirstMove handmove].int)

proc rounds: (int, int) =
  var part1, part2: int
  for line in input.lines:
    let move = line[0]
    let resp = parseEnum[ResponseMove]($line[2])
    part1 += scorePart1(move, resp)
    part2 += scorePart2(move, resp)
  (part1, part2)

let (part1, part2) = rounds()
echo "total score part1: ", part1
echo "total score: ", part2
