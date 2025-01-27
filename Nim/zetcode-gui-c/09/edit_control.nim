import std/[winlean]
import ../basic_windows_05

const
  idEdit = 1
  idButton = 2

var
  hedit: Handle
proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    hedit = createWindowW(L"Edit", L"",
                          WS_VISIBLE or WS_CHILD or WS_BORDER,
                          50, 50, 150, 20, hwnd, idEdit, nil, nil)
    discard createWindowW(L"Button", L"Set",
                          WS_CHILD or WS_VISIBLE or SS_LEFT,
                          50, 75, 150, 20, hwnd, idButton, nil, nil)
  of WM_COMMAND:
    if hiword(wparam) == BN_CLICKED:
      let length = getWindowTextLengthW(hedit) + 1
      var text = newWideCString length
      discard getWindowTextW(hedit, text, length)
      discard setWindowTextW(hwnd, text)
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Edit control"
sampleWindow name, name, wndproc
