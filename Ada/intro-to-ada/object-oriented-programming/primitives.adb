with Ada.Text_IO; use Ada.Text_IO;
with myutils;

procedure primitives is
    package ut renames myutils;
    package Week is
        subtype Days is ut.Days;
        --type Days is new ut.Days;

        procedure Print_Day(D: Days);
    end Week;

    package body Week is
        procedure Print_Day(D: Days) is
        begin
            Put_Line(ut.CapFirst(D'image));
        end Print_Day;
    end Week;

    use Week;

    subtype Weekend_Days is Days range ut.Saturday .. ut.Sunday;
    --type Weekend_Days is new Days range ut.Saturday .. ut.Sunday;

    Sat : Weekend_Days := ut.Saturday;
begin
    Print_Day(Sat);
end;