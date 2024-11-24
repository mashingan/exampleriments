with Ada.Text_IO; use Ada.Text_IO;

procedure inline_if is
begin
    for I in 1 .. 10 loop
        Put(I'image);
        Put_Line(if I mod 2 = 0 then " even" else " odd");
    end loop;
end;
