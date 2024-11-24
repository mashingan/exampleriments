with Ada.Text_IO; use Ada.Text_IO;

procedure character_types is
    type My_Char is ('a', 'b', 'c');

    C : Character;
    M : My_Char;
begin
    C := '?';
    M := 'a';

    C := Character'val(65);

    -- both below are invalid
    M := C;
    M := 'd';
end;
