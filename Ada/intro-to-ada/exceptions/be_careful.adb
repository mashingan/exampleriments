with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure be_careful is
    function Dangerous return Integer is
    begin
        raise Constraint_Error;
        return 42;
    end Dangerous;

begin
    declare
        A : Integer := Dangerous;
    begin
        Put_Line(A'image);
    exception
        when Constraint_Error => Put_Line("Error!");
    end;
end be_careful;
