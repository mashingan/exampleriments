with Ada.Text_IO; use Ada.Text_IO;

procedure case_expr is
begin
    for I in 1 .. 10 loop
        Put(I'image);
        Put_Line(
            case I is
                when 1 | 3 | 5 | 7 | 9 => " odd",
                when 2 | 4 | 6 | 8 | 10 => " even");
    end loop;
end;
