import std/sugar
const
  Unlen = 256

proc getUserNameW(buf: WideCString, size: ptr int): int32
                 {.importc: "GetUserNameW", header: "<windows.h>".}

{.passL: "-ladvapi32".}

var cap = Unlen+1
var buf = newWideCString cap

dump getUserNameW(buf, cap.addr)
dump buf
dump buf.len
