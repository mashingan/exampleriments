import std/[winlean, with]
import ../basic_windows_05

var
  gcolor: ColorRef
  crCustCtrl: array[16, ColorRef]

proc showColorDialog(hwnd: Handle): ColorRef =
  var cc: ChooseColorW
  zeroMem(cc.addr, sizeof(cc))
  with cc:
    size = int32 sizeof(ChooseColorW)
    owner = hwnd
    rgbResult = gcolor
    flags = CC_FULLOPEN or CC_RGBINIT
    custColors = crCustCtrl[0].addr
  chooseColor(cc.addr)
  cc.rgbResult

proc panelProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_PAINT:
    var
      rect: Rect
      ps: PaintStruct
    discard getClientRect(hwnd, rect.addr)
    var hdc = beginPaint(hwnd, ps.addr)
    discard setBkColor(hdc, gcolor)
    extTextOut(hdc, 0, 0, ETO_OPAQUE, rect.addr, L"", 0, nil)
    discard endPaint(hwnd, ps.addr)
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

proc registerPanel(hwnd: Handle) =
  var wc = WndClassW(
    wndProc: panelProc,
    background: getStockObject(WHITE_BRUSH),
    className: L"Panel"
  )
  registerClassW(wc.addr)

var ghpanel: Handle
proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_QUIT, WM_DESTROY, WM_CLOSE:
    postQuitMessage 0
  of WM_CREATE:
    discard createWindowW(L"button", L"Color", WS_VISIBLE or WS_CHILD,
                  20, 30, 80, 25, hwnd, 1, nil, nil)
    registerPanel(hwnd)
    ghpanel = createWindowW(L"Panel", L"", WS_VISIBLE or WS_CHILD,
                            130, 30, 80, 80, hwnd, 2, nil, nil)
  of WM_COMMAND:
    gcolor = showColorDialog(hwnd)
    discard invalidateRect(ghpanel, nil, true)
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Color dialog"
sampleWindow name, name, wndProc
