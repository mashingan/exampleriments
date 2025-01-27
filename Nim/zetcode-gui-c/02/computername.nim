import std/[winlean, sugar, unicode]

{.pragma: winh, header: "<windows.h>".}

let
  maxComputerName {.importc: "MAX_COMPUTERNAME_LENGTH", winh.} : uint
  fmAllocateBuffer {.importc: "FORMAT_MESSAGE_ALLOCATE_BUFFER", winh.}: int32
  fmFromSystem {.importc: "FORMAT_MESSAGE_FROM_SYSTEM", winh.}: int32
  langNeutral {.importc: "LANG_NEUTRAL", winh.}: int32
  langSubDefault {.importc: "SUBLANG_DEFAULT", winh.}: int32

type
  ComputerNameFormat {.importc: "COMPUTER_NAME_FORMAT", winh.} = enum
    cnNetBios
    cnDnsHostName
    cnDnsDomain
    cnDnsFullyQualified
    cnPhysicalNetBios
    cnPhysicalDnsHostname
    cnPhysicalDnsDomain
    cnPhysicalDnsFullyQualified
    cnMax


proc getComputerName(buf: pointer, written: ptr int32): WINBOOL
                    {.importc: "GetComputerName", winh.}

proc localFree(handle: Handle): WiNBOOL {.importc: "LocalFree", winh.}

template makeLangId(mainlang, sublang: uint): untyped =
  {.emit: ["MAKELANGID(", mainlang, sublang, ");"].}


proc main =
  echo "in main"
  let caplen = maxComputerName + 1
  #let caplen = 128
  dump caplen
  var buf = newWideCString(int caplen)
  var namelen = int32 caplen
  dump namelen
  let cmpnamelen = getComputerName(buf.addr, namelen.addr)
  dump namelen
  dump cmpnamelen
  dump buf.len
  if cmpnamelen == 0:
    echo "failed to get computer name"
    var msgbuf = newWideCString(128)
    let code = getLastError()
    dump code
    dump formatMessageW(fmFromSystem or fmAllocateBuffer, nil, code, langNeutral,
                           msgbuf.addr, int32 caplen, nil)
    dump msgbuf
    localFree(msgbuf.addr)
  else:
    echo "Computer name is: ", buf

main()
