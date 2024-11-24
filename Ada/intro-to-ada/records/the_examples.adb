with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

procedure the_examples is
    package SCon renames Ada.Strings.Maps.Constants;
    package SFix renames Ada.Strings.Fixed;

    type Month_Type is (
        January, February, March, April, May, June, 
        July, August, September, November, December);

    type Date is record
        Day : Integer range 1 .. 31;
        Month : Month_Type := January;
        --                   ^ default value

        Year : Integer range 1 .. 3000 := 2020;
        --                              ^ default value

    end record;

    Ada_Birthday : Date := (10, December, 1815);
    Leap_Day_2020 : Date := (
        Day => 29,
        Month => February,
        Year => 2020);

    Some_Day : Date := (1, January, 2000);

    function CapFirst (s: string) return string;
    procedure check_day(tdate: Date) is
    begin
        Put_Line(
            "Day:" & tdate.day'image &
            ", Month: " & capfirst(tdate.month'image) &
            ", Year:" & tdate.year'image);
    end check_day;

    function CapFirst (s: String) return string is
        capstr : string := SFix.translate(s(s'first .. s'first), scon.upper_case_map);
        reststr: string := sfix.translate(s(s'first+1..s'last), scon.lower_case_map);
    begin
        return capstr & reststr;
    end CapFirst;

begin
    check_day(some_day);
    Some_Day.Year := 2001;
    check_day(some_day);
    check_day(Ada_Birthday);
end;
