-- compile with: gnatmake -I.. simple_decl
with Ada.Text_IO; use Ada.Text_IO;
with myutils; use myutils;

procedure unconstrained_arr is
    type Workload_type is array (Days range <>) of Natural;

    Workload : constant Workload_type (Monday .. Friday) := (
        Friday => 7, others => 8);

    -- string as unbounded array definition
    --type String is array (Positive range <>) of Character;

    -- below, a and b is equivalent
    A : string (1 .. 16) := "hello nice world";
    B : string (1 .. 16) := (
        'h', 'e', 'l', 'l', 'o', ' ',
        'n', 'i', 'c', 'e', ' ',
        'w', 'o', 'r', 'l', 'd');
    Message : constant string := "dlrow ecin olleh";

    function Get_Day_Name( Day: Days := Monday) return String is
    begin
        return
        (case Day is
            when Monday => "Monday",
            when Tuesday => "Tuesday",
            when Wednesday => "Wednesday",
            when Thursday => "Thursday",
            when Friday => "Friday",
            when Saturday => "Saturday",
            when Sunday => "Sunday");
    end Get_Day_Name;

    Names : constant Days_Name_Type := (
        "Mo", "Tu", "We", "Th", "Fr", "Sa", "Su");

begin
    for I in workload'range loop
        Put(Integer'image (Workload(I)));
    end loop;
    New_Line;
    for I in reverse Message'range loop
        Put(Message(I));
    end loop;
    New_Line;
    Put_Line("Is A and B the same string? " & CapFirst(Boolean'image(A = B)));

    Put_Line("First day is " & Get_Day_Name(Days'first));
    Put_Line("The thursday is " & Get_Day_Name(Thursday));

    for I in Names'range loop
        Put(Names(I) & (
            if I = Names'last then "" else ", "));
    end loop;
end;
