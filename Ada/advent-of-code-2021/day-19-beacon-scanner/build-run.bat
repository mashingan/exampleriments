rem gnatmake -O2 day_19_beacon_scanner.adb -bargs -largs
gprbuild -P default.gpr
obj\debug\day_19_beacon_scanner.exe < input.txt
