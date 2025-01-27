# only works with gcc
import std/[winlean, sugar]

{.emit: """/*INCLUDESECTION*/
#include<initguid.h>
""".}

{.pragma: winh, header: "<windows.h>".}
{.pragma: knoh, header: "<knownfolders.h>".}
{.pragma: shlh, header: "<shlobj.h>".}

type
  Guid {.importc: "GUID", knoh.} = object

let
  foldidDocuments {.importc: "FOLDERID_Documents", knoh.}: Guid

proc shGetKnownFolderPath(id: ptr Guid, flags: int32, tokenopt: Handle, outstr: cstring): int32
  {.importc: "SHGetKnownFolderPath", shlh.}

{.passL: "-lole32".}
var outstr: cstring = newString(MAX_PATH)
dump shGetKnownFolderPath(foldidDocuments.unsafeAddr, 0, 0, outstr)
dump outstr.len
dump outstr
