from std/sugar import dump
from std/strutils import startsWith, splitWhitespace, contains,
     parseInt, indent
from std/tables import initTable, Table, `[]`, contains, `[]=`,
     pairs, mpairs
from std/strformat import fmt, `&`
from std/algorithm import sort, SortOrder

const
  input {.strdefine.} = "input.txt"
  mostSize = 100_000
  totalDisk = 70_000_000
  updateSpace = 30_000_000

type
  Dir = enum
    Directory = "dir"
    File = "file"

  Filesystem = ref object
    name: string
    size: int64
    parent: Filesystem
    case kind: Dir
    of Directory:
      children: Table[string, Filesystem]
    of File:
      discard

var root = FileSystem(
  name: "/",
  kind: Directory,
  children: initTable[string, Filesystem]())

proc cd(fs: var Filesystem; to: sink string) =
  if to == "/": fs = root
  elif to == "..": fs = fs.parent
  elif to notin fs.children: return
  else:
    fs = fs.children[to]

proc listDir(fs: var Filesystem; listing: sink seq[string]) =
  for line in listing:
    let tokens = line.splitWhitespace
    let childname = tokens[1]
    if childname in fs.children: continue
    if line.startsWith "dir":
      fs.children[childname] = Filesystem(
        name: childname,
        kind: Directory,
        children: initTable[string, Filesystem](),
        parent: fs,
      )
    else:
      let filesize = parseInt(tokens[0])
      fs.size += filesize
      fs.children[childname] = Filesystem(
        name: childname,
        size: filesize,
        parent: fs,
        kind: File)
  var parent = fs.parent
  while parent != nil:
    parent.size += fs.size
    parent = parent.parent

proc printFsTree(fs: Filesystem, indent = 0) =
  echo indent(&"- {fs.name} ({fs.kind}, size={fs.size})", indent)
  if fs.kind == Directory:
    for name, child in fs.children:
      printFsTree(child, indent+2)

proc fetchSize(fs: Filesystem; total: var int64) =
  if fs.kind != Directory: return
  if fs.size <= mostSize:
    total += fs.size
  for _, child in fs.children:
    fetchSize(child, total)

proc fetchRestDirs(fs: Filesystem, rest: int64, acc: var seq[int64]) =
  if fs.kind != Directory: return
  if fs.size + rest >= updateSpace: acc.add fs.size
  for _, c in fs.children:
    fetchRestDirs c, rest, acc

proc main =
  var
    fs: Filesystem
    listing: seq[string]
    ls = false

  for line in input.lines:
    if line.startsWith("$") and "cd" in line:
      if listing.len > 0:
        fs.listDir listing
        listing = @[]
      fs.cd line.splitWhitespace()[2]
      ls = false
    elif line.startsWith("$") and "ls" in line:
      ls = true
    elif ls:
      listing.add line
  if listing.len > 0:
    fs.listDir listing
    listing = @[]

  #printFsTree(root)
  var total = 0'i64
  fetchSize root, total
  dump total
  let freeSpace = totalDisk - root.size
  dump totalDisk
  dump root.size
  dump freeSpace
  var acc: seq[int64]
  fetchRestDirs root, freeSpace, acc
  sort acc, SortOrder.Ascending
  dump acc[0]

main()
