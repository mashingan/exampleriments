# must use gcc
import std/[sugar, strformat]

{.pragma: winh, header: "<windows.h>".}

type
    MemoryStatusEx {.importc: "MEMORYSTATUSEX", winh.} = object
        length {.importc: "dwLength".}: int32
        memoryLoad {.importc: "dwMemoryLoad".}: int32
        totalPhys {.importc: "ullTotalPhys".}: int64
        availPhys {.importc: "ullAvailPhys".}: int64
        totalVirtual {.importc: "ullTotalVirtual".}: int64
        availVirtual {.importc: "ullAvailVirtual".}: int64

proc globalMemoryStatusEx(outmem: ptr MemoryStatusEx): int32
    {.importc: "GlobalMemoryStatusEx", winh.}

const
    mb = 1024 * 1024
    gb = 1024 * mb

var mem: MemoryStatusEx
mem.length = int32 sizeof(mem)
dump globalMemoryStatusEx(mem.addr)
echo &"Memory in use: {mem.memoryLoad} percent"
echo &"Total physical memory: {mem.totalPhys div gb} gb"
echo &"Free physical memory: {mem.availPhys div gb} gb"
echo &"Total virtual memory: {mem.totalVirtual div gb} gb"
echo &"Free virtual memory: {mem.availVirtual div gb} gb"
