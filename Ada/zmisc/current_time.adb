with GNAT.IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;

procedure Current_Time is
    package gio renames GNAT.IO;
    use Ada.Calendar;
    use Formatting;
    use Time_Zones;
begin
    --gio.Put_line("Current time is: " & curr'image);
    gio.Put_line("current time is " & image(Clock, Time_Zone => 7*60));
    --gio.Put_line("current time is " & now'image);
end Current_Time;
