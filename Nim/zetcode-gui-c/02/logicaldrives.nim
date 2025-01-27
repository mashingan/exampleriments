import std/[winlean, sugar]

proc getLogicalDriveStringsW(pathsize: int, drives: WideCString): int32
  {.importc: "GetLogicalDriveStringsW", header: "<windows.h>".}

template `as`(a, b: untyped): untyped =
  cast[`b`](`a`)

var drives = newWideCString MAX_PATH
let drvlen = getLogicalDriveStringsW(MAX_PATH, drives)
var curr = 0
var ptrdrv = addr(drives[0])
while curr < drvlen:
  let currlen = (ptrdrv as WideCString).len
  curr += currlen + 1
  dump ptrdrv as WideCString
  ptrdrv = ((ptrdrv as ByteAddress) +% (currlen + 1) * sizeof(ptrdrv[])) as (type ptrdrv)
