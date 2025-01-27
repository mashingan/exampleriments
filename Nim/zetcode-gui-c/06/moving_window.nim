import std/[winlean, sugar, strformat]
import ../basic_windows_05

template L(str: string): WideCString = newWideCString str

proc createLabels(parent: Handle, hsta1, hsta2: var Handle) =
  let staticname = L"static"
  discard createWindowW(staticname, L"x:", WS_CHILD or WS_VISIBLE, 10, 10, 25, 25,
                        parent, 1, nil, nil)
  hsta1 = createWindowW(staticname, L"150", WS_CHILD or WS_VISIBLE, 30, 10, 25, 25,
                        parent, 2, nil, nil)
  discard createWindowW(staticname, L"y:", WS_CHILD or WS_VISIBLE, 10, 30, 25, 25,
                        parent, 3, nil, nil)
  hsta2 = createWindowW(staticname, L"150", WS_CHILD or WS_VISIBLE, 30, 30, 25, 25,
                        parent, 4, nil, nil)

var hsta1, hsta2: Handle
proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CREATE:
    createLabels(hwnd, hsta1, hsta2)
  of WM_KEYDOWN:
    if wparam == VK_ESCAPE:
      let ret = messageBoxW(hwnd, L"Are you sure to quit?",
                            L"Confirm", MB_OKCANCEL)
      if ret == IDOK:
        discard sendMessage(hwnd, WM_CLOSE, 0, 0)

  of WM_DESTROY, WM_QUIT, WM_CLOSE:
    postQuitMessage 0
  of WM_MOVE:
    var rc: Rect
    getWindowRect(hwnd, addr rc)
    let x = newWideCString($rc.left)
    let y = newWideCString($rc.top)
    discard setWindowTextW(hsta1, x)
    discard setWindowTextW(hsta2, y)
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Moving window"
sampleWindow(name, name, wndProc)
