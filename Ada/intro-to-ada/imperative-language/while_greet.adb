with Ada.Text_IO; use Ada.Text_IO;

procedure while_greet is
    I: Integer := 1;
begin
    while I <= 5 loop
        Put_Line("Hello nice world!" & I'image);
        I := I + 1;
    end loop;
end;
