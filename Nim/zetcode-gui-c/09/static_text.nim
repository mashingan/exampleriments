import std/[winlean]
import ../basic_windows_05

let lyrics = newWideCString(
  "I know what you told me I should stay away\n" &
  "I know you said he's just a dog a stray\n" &
  "気分上々一気に沸きあがる"
)

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    discard createWindowW(L"Static", lyrics,
                          WS_CHILD or WS_VISIBLE or SS_LEFT,
                          20, 20, 300, 230, hwnd, 1, nil, nil)
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Edit control"
sampleWindow name, name, wndproc
