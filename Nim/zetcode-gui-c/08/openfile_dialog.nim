import std/[winlean, with, strformat, os]
import ../basic_windows_05

const idmFileNew = 1

var
  gedit: Handle

proc createMenubar(hwnd: Handle) =
  var
    menubar = createMenu()
    menu = createMenu()
  discard appendMenuW(menubar, MF_POPUP, cast[uint](menu), L"&File")
  discard appendMenuW(menu, MF_STRING, idmFileNew, L"&Open")
  setMenu(hwnd, menubar)

proc loadFile(filename: WideCString) =
  var file = createFileW(filename, GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0)
  var size = getFileSize(file, nil)
  var buf = heapAlloc(getProcessHeap(), HEAP_GENERATE_EXCEPTIONS, size+1)
  var byteread: int
  discard readFile(file, buf, size, byteread.addr, nil)
  discard closeHandle(file)
  discard setWindowText(gedit, cast[cstring](buf))
  discard heapFree(getProcessHeap(), 0, buf)

var szfile = newWideCString(MAX_PATH)
proc openDialog(hwnd: Handle) =
  var ofn: OpenFileNameW
  szfile[0] = Utf16Char 0
  zeroMem(ofn.addr, sizeof(ofn))
  with ofn:
    size = int32 sizeof(ofn)
    file = szfile
    owner = hwnd
    maxFile = MAX_PATH
    filter = L "Text files(*.txt)\0*.txt\0All files(*.*)\0*.*\0\0"
    filterIndex = 1
    initialDir = L""
    fileTitle = L""
    flags = OFN_PATHMUSTEXIST or OFN_FILEMUSTEXIST

  if getOpenFileNameW(addr ofn):
    loadFile(ofn.file)
    discard setWindowText(hwnd, lastPathPart $ofn.file)
  else:
    let msg = L &"Failed to open file {$ofn.file}"
    discard setWindowTextW(gedit, msg)
    discard setWindowTextW(hwnd, L"OpenDialog")

proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_QUIT, WM_DESTROY, WM_CLOSE:
    postQuitMessage 0
  of WM_CREATE:
    gedit = createWindowExW(WS_EX_RIGHTSCROLLBAR, L"Edit", L"", WS_VISIBLE or
                            WS_CHILD or WS_HSCROLL or WS_VSCROLL or ES_MULTILINE,
                            0, 0, 260, 180, hwnd, 1, nil, nil)

    createMenubar(hwnd)
  of WM_SIZE:
    setWindowPos(gedit, 0, 0, 0, loword(uint lparam), hiword(uint lparam),
                 SWP_NOMOVE or SWP_NOZORDER)
  of WM_COMMAND:
    if wparam == idmFileNew:
      openDialog hwnd
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"OpenDialog"
sampleWindow name, name, wndProc
