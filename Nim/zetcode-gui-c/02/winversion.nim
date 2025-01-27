# only works with --cc:gcc
# ref: https://learn.microsoft.com/en-us/windows/win32/api/versionhelpers/
{.emit: """/*INCLUDESECTION*/
#include<windows.h.>
"""
.}
type WinBool = distinct int32

converter toBool(wb: WinBool): bool =
  wb.int32 == 1

{.pragma: verh, header: "<versionhelpers.h>".}
proc isWindows10OrGreater: WinBool {.importc: "IsWindows10OrGreater", verh.}
proc isWindows8OrGreater: WinBool {.importc: "IsWindows8OrGreater", verh.}
proc isWindows7OrGreater: WinBool {.importc: "IsWindows7OrGreater", verh.}
proc isWindowsVistaOrGreater: WinBool {.importc: "IsWindowsVistaOrGreater", verh.}
proc isWindowsXPOrGreater: WinBool {.importc: "IsWindowsXPOrGreater", verh.}

if isWindows10OrGreater():
  echo "This is windows 10+"
elif isWindows8OrGreater():
  echo "This is windows 8"
elif isWindows7OrGreater():
  echo "This is windows 7"
elif isWindowsVistaOrGreater():
  echo "This is windows Vista"
elif isWindowsXPOrGreater():
  echo "This is windows XP"
