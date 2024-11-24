with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure case_direction is
    N : Integer;
begin
    loop
        Put("Enter an integer value: ");
        Get(N);
        Put(N);
        case N is
            when 0 | 360 =>
                Put_Line(" is due east");
            when 1 .. 89 =>
                Put_Line(" is due northeast quadrant");
            when 90 =>
                Put_Line(" is due north");
            when 91 .. 179 =>
                Put_Line(" is due northwest quadrant");
            when 180 =>
                Put_Line(" is due west");
            when 181 .. 269 =>
                Put_Line(" is due southwest quadrant");
            when 270 =>
                Put_Line(" is due south");
            when 271 .. 359 =>
                Put_Line(" is due southeast quadrant");
            when others =>
                Put_Line(" au revoir");
                exit;
        end case;
    end loop;
end case_direction;
