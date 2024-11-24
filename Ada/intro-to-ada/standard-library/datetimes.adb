with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Text_IO; use Ada.Text_IO;

procedure datetimes is
    Now : Time := Clock;

    Now_Year : Year_Number;
    Now_Month : Month_Number;
    Now_Day : Day_Number;
    Now_Seconds : Day_Duration;

    TZ : Time_Offset := UTC_Time_Offset;
    Next : Time := Ada.Calendar.Formatting.Time_Of(
        Year    => 2020,
        Month   => 5,
        Day     => 4,
        Hour    => 19,
        Minute  => 45,
        Second  => 0,
        Leap_Second => false,
        Time_Zone => TZ);
    Next2 : Time := Ada.Calendar.Formatting.Value(
        "2020-05-04 19:00:00.00", TZ);

    dur : Duration := 5.0;

begin
    Put_Line("Current time: " & image(now));
    Split(
        Now,
        Now_Year,
        Now_Month,
        Now_Day,
        Now_Seconds);
    Put_Line("Current year is " & Now_Year'image);
    Put_Line("Current month is " & Now_Month'image);
    Put_Line("Current day is " & Now_Day'image);

    Put("Let's wait until...");
    Put_Line(Image(Next, true, TZ));

    delay until next;

    Put_Line("Enough!");
    --now := clock;
    --Put_Line("Current time: " & image(now));
    Put_Line("Current time: " & image(clock));

    Put("Let's wait " & Duration'image(dur) & " seconds...");
    next := clock + dur;
    delay until next;
    Put_Line("Done!");
end;
