--https://stackoverflow.com/a/34686779
--gnatmake -O2 hello.adb -bargs -shared -largs -s
with GNAT.IO;
use GNAT.IO;
--with Ada.Text_IO;
--use Ada.Text_IO;

procedure Hello is
begin
    Put_line("Hello world!");
end Hello;
