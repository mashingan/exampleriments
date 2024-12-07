from std/sugar import dump

const input {.strdefine.} = "input.txt"

func findMarker(line: sink string; uniqueTotal = 4): int =
  var pos = 0
  while pos < line.len-uniqueTotal:
    let bound = min(pos+uniqueTotal-1, line.len-1)
    var sliceset: set[char]
    for c in line[pos .. bound]: sliceset.incl c
    if sliceset.len == uniqueTotal: return bound+1
    inc pos

proc main =
  for line in input.lines:
    dump line.findMarker
    dump line.findMarker 14

main()
