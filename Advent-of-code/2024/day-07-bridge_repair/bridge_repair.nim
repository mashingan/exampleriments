#from strutils import parseInt, split
import std/[strutils, sequtils, hashes, strformat]
import bigints
from graflib import Edge, buildGraph, `a*`, `<`

type Node = object
    paths: seq[int]
    deep: int
    current: BigInt
    op: char
    total: BigInt

func hash(d: Node): Hash =
    var h: Hash = 0
    h = h !& hash(d.total)
    result = !$h

func `==`(n1, n2: Node): bool = n1.current == n2.current and n1.total == n2.total

func cost(a, b: Node): int = 1
# func distance(a, b: Node): int = 1
func distance(a, b: Node): int =
    if b.op == '+':
        if b.total - initBigInt(b.paths[b.deep]) < initBigInt(a.current): int.high
        else:
            b.deep
    else:
        let rem = b.total mod b.current
        if rem > 0'bi: int.high
        else:
            b.deep

proc next(a: Node, edges: seq[Edge[Node]]): seq[Node] =
    if a.deep + 1 > a.paths.high: return @[]
    var currentp = a.current
    var currentm = a.current
    if a.deep+1 <= a.paths.high:
        if a.deep == 0:
            currentp = initBigInt(a.paths[0] + a.paths[1])
            currentm = initBigInt(a.paths[0] * a.paths[1])
        else:
            currentp += initBigInt(a.paths[a.deep+1])
            currentm *= initBigInt(a.paths[a.deep+1])
    @[Node(
        deep: a.deep+1,
        current: currentp,
        paths: a.paths,
        total: a.total,
        op: '+',
    ), Node(
        deep: a.deep+1,
        current: currentm,
        paths: a.paths,
        total: a.total,
        op: '*',
    )]

const inputfile {.strdefine.} = "sample.txt"

proc main =
    var sum = 0'bi
    var count = 0
    var validcount = 0
    for ln in inputfile.lines:
        var g = buildGraph[Node](directed = true)
        let ll = ln.split(sep = ':', maxSplit = 2)
        let total = initBigInt(ll[0])
        # echo ll
        # echo ll[1].split.mapIt(it.strip).filterIt(it != "").map(parseInt)
        let start = Node(
            paths: ll[1].split.mapIt(it.strip).filterIt(it != "").map(parseInt),
            total: total,
        )
        let goal = Node(
            paths: start.paths,
            current: start.total,
            total: total,
        )
        var paths = `a*`[Node, int](g, start, goal)
        inc count
        if paths.len > 0:
            inc validcount
            let last = initBigInt(start.paths[^1])
            if paths[^1].current div last == paths[paths.high-1].current:
                paths[^1].op = '*'
            elif paths[^1].current - last == paths[paths.high-1].current:
                paths[^1].op = '+'
            # echo paths
            sum += total
            echo &"line: {count}, the sum: {total}, current sum: {sum}"
    echo sum
    echo "valid count: ", validcount

main()