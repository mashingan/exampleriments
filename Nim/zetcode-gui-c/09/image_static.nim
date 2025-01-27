import std/[winlean, with, strformat, os]
import ../basic_windows_05

const idmFileNew = 1

var
  gedit: Handle
  gbitmap: Handle

proc createMenubar(hwnd: Handle) =
  var
    menubar = createMenu()
    menu = createMenu()
  discard appendMenuW(menubar, MF_POPUP, cast[uint](menu), L"&File")
  discard appendMenuW(menu, MF_STRING, idmFileNew, L"&Open")
  setMenu(hwnd, menubar)

proc loadFile(filename: WideCString) =
  #[
  var bitmap: Bitmap
  var ptrbitmap = addr bitmap
  discard sendMessage(gedit, STM_SETIMAGE, IMAGE_BITMAP, ptrbitmap)
  ]#
  gbitmap = loadImageW(nil, filename, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE)
  discard sendMessage(gedit, STM_SETIMAGE, IMAGE_BITMAP, cast[cint](gbitmap))

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
    #[
    filter = newWideCString(
      "Bitmap files(*.bmp)\0*.bmp\0" &
      "Jpg files(*.jpg;*.jpeg)\0*.jpg;*.jpeg\0" &
      "Png files(*.png)\0*.png\0" &
      "All files(*.*)\0*.*\0\0")
    ]#
    filter = L "Bitmap files(*.bmp)\0*.bmp\0"
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
    discard deleteObject(gbitmap)
    postQuitMessage 0
  of WM_CREATE:
    gedit = createWindowW(L"Static", L"", WS_VISIBLE or WS_CHILD or SS_BITMAP,
                            5, 5, 300, 300, hwnd, 1, nil, nil)

    createMenubar(hwnd)
  of WM_SIZE:
    discard
    #setWindowPos(gedit, 0, 5, 5, loword(uint lparam) - 5, hiword(uint lparam) - 5,
                 #SWP_NOMOVE or SWP_NOZORDER)
  of WM_COMMAND:
    if wparam == idmFileNew:
      openDialog hwnd
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Static image"
sampleWindow name, name, wndProc
