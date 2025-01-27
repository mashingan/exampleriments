import std/[winlean, colors, sugar]
import ../basic_windows_05

const
  idBlue = 1
  idYellow = 2
  idOrange = 3

var
  hinst: Hinstance
  gcolor: ColorRef

proc createControl(hwnd: Handle) =
  discard createWindowW(L"Button", L"Choose color", WS_CHILD or WS_VISIBLE or BS_GROUPBOX,
                        10, 10, 120, 110, hwnd, 0, hinst, nil)
  discard createWindowW(L"Button", L"Blue", WS_CHILD or WS_VISIBLE or BS_AUTORADIOBUTTON,
                        20, 30, 100, 30, hwnd, idBlue, hinst, nil)
  discard createWindowW(L"Button", L"Yellow", WS_CHILD or WS_VISIBLE or BS_AUTORADIOBUTTON,
                        20, 55, 100, 30, hwnd, idYellow, hinst, nil)
  discard createWindowW(L"Button", L"Orange", WS_CHILD or WS_VISIBLE or BS_AUTORADIOBUTTON,
                        20, 80, 100, 30, hwnd, idOrange, hinst, nil)

proc paintWindow(hwnd: Handle) =
  var
    ps: PaintStruct
    hdc = beginPaint(hwnd, ps.addr)
    hbrush = createSolidBrush(gcolor)
    holdbrush = selectObject(hdc, hbrush)
    hpen = createPen(PS_NULL, 1, ColorRef colBlack)
    holdpen = selectObject(hdc, hpen)

  discard rectangle(hdc, 160, 20, 260, 120)
  discard selectObject(hdc, holdbrush)
  discard selectObject(hdc, holdpen)
  discard deleteObject(hpen)
  discard deleteObject(hbrush)
  discard endPaint(hwnd, ps.addr)

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_QUIT, WM_CLOSE, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    createControl(hwnd)
  of WM_COMMAND:
    if hiword(wparam) == BN_CLICKED:
      case loword(wparam)
      of idBlue:
        #gcolor = ColorRef colBlue
        static:
          dump colBlue
          dump ColorRef colBlue
        gcolor = rgb(0, 76, 255)
      of idYellow:
        static:
          dump colYellow
          dump ColorRef colYellow
        #gcolor = ColorRef colYellow
        gcolor = rgb(255, 255, 0)
      of idOrange:
        static:
          dump colOrange
          dump ColorRef colOrange
        #gcolor = ColorRef colOrange
        gcolor = rgb(255, 123, 0)
      else: discard
      discard invalidateRect(hwnd, nil, true)
  of WM_PAINT:
    paintWindow hwnd
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

hinst = getModuleHandle ""
let name = L"Radio buttons"
sampleWindow name, name, wndproc
