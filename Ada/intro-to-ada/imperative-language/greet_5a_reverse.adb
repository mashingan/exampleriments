with Ada.Text_IO; use Ada.Text_IO;

procedure Greet_5a_reverse is
begin
    for I in reverse 1 .. 5 loop
        Put_Line("Hello nice world!" & I'image);
    end loop;
end Greet_5a_reverse;
