with Ada.Text_IO; use Ada.Text_IO;

procedure integer_type is
    type MyInt is range -1 .. 20;

    type Builtin_Integer is range -(2 ** 31) .. +(2 ** 31 - 1);

begin
    for I in myint loop
        Put_Line(I'image);
    end loop;
end;
