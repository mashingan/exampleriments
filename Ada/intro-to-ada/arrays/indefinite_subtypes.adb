with Ada.Text_IO; use Ada.Text_IO;

procedure Indefinite_Subtypes is
    function Get_Number return Integer is
    begin
        return Integer'value(Get_Line);
    end Get_Number;

    A : string := "Hello"; -- indefinite subtypes

    B : string (1 .. 5) := "Hello"; -- definite subtypes

    -- indefinite subtypes as Get_Number computed at run-time
    C : String (1 .. Get_Number);
begin
    null;
end;
