import std/[winlean, sugar, os]

var buf = newWideCString(MAX_PATH)
let params = commandLineParams()
if params.len < 1:
  quit "Usage: current_dir <path>"

dump setCurrentDirectoryW(newWideCString(params[0]))
dump getCurrentDirectoryW(MAX_PATH, buf)
echo "curent dir is ", $buf
