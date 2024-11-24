with GNAT.IO;
--with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting;

procedure Sleep3Min is
    use Ada.Calendar;
    use Time_Zones;
    use Formatting;
    function now return string is
    begin
        return image(Clock, Time_Zone => 7 * 60);
    end now;
begin
    GNAT.IO.Put_line("Start at " & now);
    --Ada.Text_IO.Put_line("Start at " & now);
    delay(180.0);
    GNAT.IO.Put_line("End at " & now);
    --Ada.Text_IO.Put_line("End at " & now);
end Sleep3Min;
