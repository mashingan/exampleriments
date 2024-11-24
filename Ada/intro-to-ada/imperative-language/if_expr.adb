with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure if_expr is
    N : integer;
begin
    Put("Enter an integer value: ");
    Get(N);
    Put(N);
    declare
        S : string := (
            if N > 0 then " is a positive number"
            else " is not a positive number");
    begin
        Put_Line(S);
    end;
end if_expr;
