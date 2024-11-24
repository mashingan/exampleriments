with Ada.Text_IO; use Ada.Text_IO;

procedure nested is
    procedure inner_nested is
    begin
        Put_Line("This is inner nested");
    end inner_nested;
begin
    inner_nested;
    Put_Line("This is outer!");
end nested;
