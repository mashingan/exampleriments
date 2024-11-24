with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Ada.Text_IO; use Ada.Text_IO;

procedure hashed_maps is
    package Integer_Hashed_Maps is new
        Ada.Containers.Indefinite_Hashed_Maps (
            Key_Type => String,
            Element_Type => Integer,
            Hash => Ada.Strings.Hash,
            Equivalent_Keys => "=");

    use Integer_Hashed_Maps;

    M : Map;

begin
    M.Include("Alice", 24);
    M.Include("John", 40);
    M.Include("Bob", 28);

    if M.Contains("Alice") then
        Put_Line("Alice's age is " & Integer'image(M ("Alice")));
    end if;

    -- Key must already exists in M otherwise an exception raised.
    M("Alice") := 25;

    Put_Line("Name & Age:");
    for C in M.Iterate loop
        Put_Line(Key(C) & ": " & Integer'image(M (C)));
    end loop;
end;
