import std/[winlean, with, sequtils, strformat]
import ../basic_windows_05

let cap = [75, 150, 225, 300, 375, 450, 525, 600, 675].mapIt( L $it )
var gpos = 150

proc panelProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_PAINT:
    var
      ps: PaintStruct
      rect, rect2: Rect
      hdc = hwnd.beginPaint ps.addr

      hbrushYellow = createSolidBrush(rgb(255, 255, 184))
      hbrushRed = createSolidBrush(rgb(255, 110, 110))
      holdbrush = selectObject(hdc, hbrushYellow)
      hpen = createPen(PS_NULL, 1, rgb(0, 0, 0))
      holdpen = selectObject(hdc, hpen)
      hfont = createFontW(13, 0, 0, 0, FW_MEDIUM, false, false, false,
                          0, 0, 0, 0, 0, L"Tahoma")
      holdfont = selectObject(hdc, hfont)

    discard getClientRect(hwnd, rect.addr)
    let
      till = int((rect.right.float / 750.0) * gpos.float)
      step = (rect.right.float / 10.0).int
      full = int((rect.right.float / 750.0) * 700.float)

    if till > full:
      discard selectObject(hdc, hbrushYellow)
      discard rectangle(hdc, 0, 0, full, 30)
      holdbrush = selectObject(hdc, hbrushRed)
      discard rectangle(hdc, full, 0, till, 30)
    else:
      holdbrush = selectObject(hdc, hbrushYellow)
      discard rectangle(hdc, 0, 0, till, 30)

    discard selectObject(hdc, holdpen)
    for i in 1 ..< 10:
      let x = i * step
      discard hdc.moveToEx(x, 0, nil)
      discard hdc.lineTo(x, 7)

      with rect2:
        bottom = 28
        top = 8
        left = clong(x - 10)
        right = clong(x + 10)

      discard setBkMode(hdc, TRANSPARENT)
      let text = cap[i-1]
      discard drawTextW(hdc, text, text.len, rect2.addr, DT_CENTER)

    discard selectObject(hdc, holdbrush)
    discard deleteObject(hbrushYellow)
    discard deleteObject(hbrushRed)
    discard deleteObject(hpen)
    discard selectObject(hdc, holdfont)
    discard deleteObject(hfont)
    discard endPaint(hwnd, ps.addr)
  else: discard
  defWindowProcW(hwnd, msg, wparam, lparam)

var
  htrack, hburn: Handle
  ghinst: Hinstance

proc createControl(hwnd: Handle) =
  let burnname = L"BurningControl"
  var icx = InitCommonControlsEx(
    size: int32 sizeof(InitCommonControlsEx),
    icc: ICC_BAR_CLASSES,
  )
  initCommonControlsEx(addr icx)
  var wc: WndClassW
  zeroMem(addr wc, sizeof(wc))
  with wc:
    className = burnname
    style = CS_HREDRAW
    wndProc = panelProc
    cursor = loadCursor(0, idcArrow)
    background = getSysColorBrush(COLOR_BTNFACE)
  registerClassW(addr wc)

  hburn = createWindowExW(WS_EX_STATICEDGE, burnname, L"", WS_CHILD or WS_VISIBLE,
                          0, 330, 490, 30, hwnd, 1, ghinst, nil)
  htrack = createWindowW(L TRACKBAR_CLASS, L"", WS_CHILD or WS_VISIBLE or
                         TBS_FIXEDLENGTH or TBS_NOTICKS,
                         40, 25, 150, 25, hwnd, 2, ghinst, nil)
  discard sendMessage(htrack, TBM_SETRANGE, 1, makeLong(0, 750))
  discard htrack.sendMessage(TBM_SETPAGESIZE, 0, 20)
  discard htrack.sendMessage(TBM_SETTICFREQ, 20, 0)
  discard htrack.sendMessage(TBM_SETPOS, 1, 150)

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_QUIT, WM_CLOSE, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    createControl(hwnd)
  of WM_SIZE:
    setWindowPos(hburn, 0, 0, hiword(cuint lparam)-30,
                 loword(cuint lparam), 30, SWP_NOZORDER)
  of WM_HSCROLL:
    gpos = sendMessage(htrack, TBM_GETPOS, 0, 0)
    let label = &"Burning control, pos: {gpos}"
    discard hwnd.setWindowText(label)
    discard invalidateRect(hburn, nil, true)
  else: discard
  defWindowProcW(hwnd, msg, wparam, lparam)

ghinst = getModuleHandle ""
let name = L"Burning control"
sampleWindow name, name, wndproc
