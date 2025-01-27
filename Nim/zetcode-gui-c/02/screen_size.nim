const
  smCxScreen = 0
  smCyScreen = 1

proc getSystemMetrics(index: int): int {.importc: "GetSystemMetrics", header: "<windows.h>".}

{.passL: "-luser32".}
let x = getSystemMetrics smCxScreen
let y = getSystemMetrics smCyScreen
echo "The screen size is: ", x, "x", y
