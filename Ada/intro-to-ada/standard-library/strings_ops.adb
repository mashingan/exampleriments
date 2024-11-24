with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;

procedure strings_ops is
    S : String := "Hello " & 3 * "nice " & "world";
    p : constant String := "nice";
    idx : natural;
    cnt : natural;

    F : Positive;
    L : Natural;
    I : Natural := 1;

    whitespace : constant Character_Set := To_set(' ');
begin
    Cnt := Ada.Strings.Fixed.Count
    (Source => S, Pattern => P);

    Put_Line("String: " & S);
    Put_Line("Count for '" & P & "': " & Cnt'image);

    Idx := 0;
    for I in 1 .. cnt loop
        idx := Index(
            Source => S,
            Pattern => P,
            From => idx + 1);
        Put_Line("Found instance of '" & p & "' at position: " & idx'image);
    end loop;

    Put_Line("String: '" & S & "', with length: " & Integer'image(S'length));

    while I in S'range loop
        Find_Token(
            Source => S,
            Set => Whitespace,
            From => I,
            Test => Outside,
            First => F,
            Last => L);
        exit when L = 0;

        Put_Line(
            "Found word instance at position " &
            F'image &
            ": '" & S(F .. L) & "'");
        I := L + 1;
    end loop;
end;
