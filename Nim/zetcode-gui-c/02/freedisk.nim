import std/[sugar, strformat]

{.pragma: winh, header: "<windows.h>".}

proc getLogicalDriveStringsW(pathsize: int, drives: WideCString): int32
  {.importc: "GetLogicalDriveStringsW", winh.}

proc getDiskFreeSpaceExW(drive: WideCString, freecall, total, free: ptr uint64): int32
  {.importc: "GetDiskFreeSpaceExW", winh.}

proc reportDisk(drive: WideCString) =
  var freecall, total, free: uint64
  dump getDiskFreeSpaceExW(drive, freecall.addr, total.addr, free.addr)
  const gb = 1024 * 1024 * 1024
  echo &"=========report on drive {drive}========"
  echo &"Available space to caller {freecall div gb} GB"
  echo &"Total space {total div gb} GB"
  echo &"Free space {free div gb} GB"

let
  MAX_PATH {.importc, winh.}: int32

template `as`(a, b: untyped): untyped =
  cast[`b`](`a`)

var drives = newWideCString MAX_PATH
let drvlen = getLogicalDriveStringsW(MAX_PATH, drives)
var curr = 0
var ptrdrv = addr(drives[0])
while curr < drvlen:
  let currlen = (ptrdrv as WideCString).len
  curr += currlen + 1
  #dump ptrdrv as WideCString
  reportDisk(ptrdrv as WideCString)
  ptrdrv = ((ptrdrv as ByteAddress) +% (currlen + 1) * sizeof(ptrdrv[])) as (type ptrdrv)
