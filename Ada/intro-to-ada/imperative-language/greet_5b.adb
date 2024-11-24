with Ada.Text_IO; use Ada.Text_IO;

procedure greet_5b is
    I : Integer := 1;
begin
    loop
        Put_Line("Hello nice world!" & I'image);
        exit when I = 5;
        I := I + 1;
    end loop;
end greet_5b;
