import std/[winlean, sugar]
import ../basic_windows_05

proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_KEYDOWN:
    if wparam == VK_ESCAPE:
      let ret = messageBoxW(hwnd, newWideCString("Are you sure to quit?"),
                            newWideCString("Confirm"), MB_OKCANCEL)
      if ret == IDOK:
        discard sendMessage(hwnd, WM_CLOSE, 0, 0)

  of WM_DESTROY, WM_QUIT, WM_CLOSE:
    postQuitMessage 0
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = newWideCString("Escape key")
sampleWindow(name, name, wndProc)
