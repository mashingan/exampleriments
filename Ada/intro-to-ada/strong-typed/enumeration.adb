with Ada.Text_IO; use Ada.Text_IO;

procedure enumeration is
    type Day is (
        Monday, Tuesday, Wednesday, Thursday,
        Friday, Saturday, Sunday);

begin
    for I in Days loop
        case I is
            when Saturday .. Sunday =>
                Put_Line(I'Image & " Week end!");
            when Monday .. Friday =>
                Put_Line(I'Image & " work day!");
        end case;
    end loop;
end;
