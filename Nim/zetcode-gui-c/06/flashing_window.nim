import std/[winlean, sugar]
import ../basic_windows_05

proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CREATE:
    discard createWindowW(newWideCString("Button"), newWideCString("Flash"),
                          WS_CHILD or WS_VISIBLE, 10, 10, 80, 25, hwnd, 1, nil, nil)
  of WM_KEYDOWN:
    if wparam == VK_ESCAPE:
      let ret = messageBoxW(hwnd, newWideCString("Are you sure to quit?"),
                            newWideCString("Confirm"), MB_OKCANCEL)
      if ret == IDOK:
        discard sendMessage(hwnd, WM_CLOSE, 0, 0)

  of WM_DESTROY, WM_QUIT, WM_CLOSE:
    postQuitMessage 0
  of WM_COMMAND:
    var fwi: FlashwInfo
    fwi.size = uint sizeof(fwi)
    fwi.flags = FLASHW_TRAY
    fwi.hwnd = hwnd
    fwi.count = 4
    flashWindowEx(fwi.addr)
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = newWideCString("Flashing window")
sampleWindow(name, name, wndProc)
