import std/[winlean, os]


proc writeConsole(std: Handle, text: cstring, textlen: uint,
                  written: ptr int32, unused: pointer): WINBOOL
                 {.importc: "WriteConsole", header: "<windows.h>".}
proc main =
  let std = getStdHandle STD_OUTPUT_HANDLE
  if std == INVALID_HANDLE_VALUE:
    quit "Cannot get STD_OUTPUT_HANDLE"
  var written: int32
  let params = commandLineParams()
  if params.len > 0:
    discard writeConsole(std, params[0], params[0].len.uint, written.addr, nil)
  discard closeHandle(std)

main()
