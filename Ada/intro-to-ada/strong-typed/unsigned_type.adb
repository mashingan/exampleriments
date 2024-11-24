with Ada.Text_IO; use Ada.Text_IO;

procedure unsigned_type is
    type Mod_Int is mod 2 ** 5;

    A : Mod_Int := 20;
    B : Mod_Int := 15;
    M : Mod_Int := A + B;
    -- no overflow, M = (20 + 15) mod 32 = 3

begin
    for I in 1 .. M loop
        Put_Line("hello world" & I'image);
    end loop;
end;
