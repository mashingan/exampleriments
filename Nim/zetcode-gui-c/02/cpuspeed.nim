import std/[winlean, sugar, strutils]

{.pragma: winh, header: "<windows.h>".}

let
  hkeyClassesRoot {.importc: "HKEY_CLASSES_ROOT", winh.}: Handle
  hkeyCurrentUser {.importc: "HKEY_CURRENT_USER", winh.}: Handle
  hkeyLocalMachine {.importc: "HKEY_LOCAL_MACHINE", winh.}: Handle
  hkeyUsers {.importc: "HKEY_USERS", winh.}: Handle
  keyRead {.importc: "KEY_READ", winh.}: int32
  fmAllocateBuffer {.importc: "FORMAT_MESSAGE_ALLOCATE_BUFFER", winh.}: int32
  fmFromSystem {.importc: "FORMAT_MESSAGE_FROM_SYSTEM", winh.}: int32
  langNeutral {.importc: "LANG_NEUTRAL", winh.}: int32

{.passL: "-ladvapi32".}

proc regOpenKeyEx(key: Handle, path: WideCString, reserved: int32, accmask: int32,
                  keyhandle: ptr Handle): int32 {.importc: "RegOpenKeyExW", winh.}

proc regQueryValueEx(key: Handle, query: WideCString, reserved: ptr int32,
                     buffaddr: pointer, valbuf: pointer, bufaddrsize: ptr int32): int32
                    {.importc: "RegQueryValueExW", winh.}

proc regCloseKey(key: Handle): int32 {.importc: "RegCloseKey", winh.}

proc localFree(handle: Handle): WiNBOOL {.importc: "LocalFree", winh.}

var bufsize = int32 MAX_PATH
var mhz = MAX_PATH
var hkey: Handle

var path = newWideCString("HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0")
dump path
var r = regOpenKeyEx(hkeyLocalMachine, path, 0, keyRead, hkey.addr)
dump r

if r != 0:
  echo "error to open reg"
  var msgbuf = newWideCString(128)
  let code = r
  dump code
  dump code.toHex
  dump formatMessageW(fmFromSystem or fmAllocateBuffer, nil, code, langNeutral,
                      msgbuf.addr, 128, nil)
  quit $msgbuf

var query = newWideCString("~MHZ")
r = regQueryValueEx(hkey, query, nil, nil, mhz.addr, bufsize.addr)
dump r
echo "cpu speed is ", mhz, " MHZ"
dump regCloseKey(hkey)
