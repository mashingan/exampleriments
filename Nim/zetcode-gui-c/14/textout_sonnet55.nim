import std/[winlean, sequtils]
import ../basic_windows_05

const lyrics = [
  "Not marble, nor the gilded monuments",
  "Of princes, shall outlive this powerful rhyme",
  "But you shal shine more bright in these contents",
  "Than unswept stone, besmear'd with sluttish time",
  "When wasteful war shall statues overturn,",
  "And broils root out the work of masonry,",
  "Nor mars his sword, nor war's quick fire shall burn",
  "The living record of your memory.",
  "'Gaints death, and all oblivious enmity",
  "Shall you pace forth; your praise shall till find room",
  "Even in the eyes of all posterity",
  "That wear this world out to the ending doom.",
  "So, till the judgement that yourself arise,",
  "You live in this, and dwell in lovers' eyes.",
]


proc drawLines(hwnd: Handle) =
  var
    ps: PaintStruct
    hdc = hwnd.beginPaint ps.addr
    color = getSysColor(COLOR_BTNFACE)
    hfont = createFontW(15, 0, 0, 0, FW_MEDIUM, false, false, false,
                        0, 0, 0, 0, 0, L"Georgia")
                        #0, 0, 0, 0, 0, L"Comic Mono")
                        #0, 0, 0, 0, 0, L"Consolas Mono")

    holdfont = hdc.selectObject hfont
  discard hdc.setBkColor color
  for i, s in lyrics:
    discard hdc.textOutW(50, 20 * (i+1), L s, s.len)
  discard hdc.selectObject holdfont
  discard deleteObject hfont
  discard hwnd.endPaint ps.addr

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY: postQuitMessage 0
  of WM_PAINT:
    drawLines(hwnd)
  else: discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Sonnet 55"
sampleWindow name, name, wndproc
