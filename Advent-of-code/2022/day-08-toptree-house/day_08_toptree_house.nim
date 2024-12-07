from std/sequtils import anyIt, toSeq
from std/sugar import dump
from std/times import cpuTime

const wthread = compileOption("threads")
when wthread:
  from std/algorithm import reversed
  from std/threadpool import spawn, `^`

const input {.strdefine.} = "input.txt"

when wthread:
  func calcScenicPoints(s: string, therange: seq[int], c: char): int {.thread.} =
    for i in therange:
      inc result
      if s[i] >= c: break

proc survey(themap: seq[string]): (int, int) =
  var maptranspose = newseq[string](themap[0].len)
  for s in themap:
    for i, c in s:
      maptranspose[i] &= c

  when not wthread:
    template applyScenicPoints(s: string, pos: int, scenicPoints: var int) =
      inc scenicPoints
      if s[pos] >= c: break

  var
    unobstructedTrees = 0
    maxScenicPoint = int.low
  for j in themap.low+1 .. themap.high-1:
    let therow = themap[j]
    for i in therow.low+1 .. therow.high-1:
      let
        c = therow[i]
        leftObstructed = therow[therow.low ..< i].anyIt(it >= c)
        rowMinbound = min(i+1, therow.len-1)
        rightObstructed = therow[rowMinbound .. ^1].anyIt(it >= c)
        column = maptranspose[i]
        topObstructed = column[column.low ..< j].anyIt(it >= c)
        colMinbound = min(j+1, column.len-1)
        bottomObstructed = column[colMinbound .. ^1].anyIt(it >= c)

      if not (leftObstructed and rightObstructed and topObstructed and bottomObstructed):
        inc unobstructedTrees
      when wthread:
        let
          left = spawn calcScenicPoints(therow, toSeq(therow.low .. i-1).reversed, c)
          right = spawn calcScenicPoints(therow, toSeq(rowMinbound .. therow.high), c)
          top = spawn calcScenicPoints(column, toSeq(column.low .. j-1).reversed, c)
          bottom = spawn calcScenicPoints(column, toSeq(colMinbound .. column.high), c)
          leftScenicPoints = ^left
          rightScenicPoints = ^right
          topScenicPoints = ^top
          bottomScenicPoints = ^bottom
      else:
        var
          leftScenicPoints = 0
          rightScenicPoints = 0
          topScenicPoints = 0
          bottomScenicPoints = 0
        for ileft in countdown(i-1, therow.low):
          therow.applyScenicPoints ileft, leftScenicPoints
        for iright in rowMinbound .. therow.high:
          therow.applyScenicPoints iright, rightScenicPoints
        for jtop in countdown(j-1, column.low):
          column.applyScenicPoints jtop, topScenicPoints
        for jdown in colMinbound .. column.high:
          column.applyScenicPoints jdown, bottomScenicPoints
      let currentScenicPoints = leftScenicPoints * rightScenicPoints *
                                topScenicPoints * bottomScenicPoints
      maxScenicPoint = max(maxScenicPoint, currentScenicPoints)
  (unobstructedTrees, maxScenicPoint)

proc main =
  var
    countVisible = 0
    map: seq[string]
  for line in input.lines:
    map.add line
  let start = cpuTime()
  let (unobstructedTrees, maxScenicPoint) = survey map
  countVisible += 2 * map[0].len + 2 * (map.len - 2) + unobstructedTrees
  dump countVisible
  dump maxScenicPoint
  dump cpuTime() - start

main()
