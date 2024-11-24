with Ada.Text_IO; use Ada.Text_IO;

procedure select_loop is
    task T is
        entry Reset;
        entry Increment;
    end T;

    task Body T is
        Cnt : Integer := 0;
    begin
        loop
            select
                accept Reset do
                    cnt := 0;
                end Reset;
                Put_Line("Reset");
            or
                accept Increment do
                    cnt := cnt + 1;
                end Increment;
                Put_Line("In T's loop (" & Cnt'image & ")");
            or
                terminate;
            end select;
        end loop;
    end T;

begin
    Put_Line("In main");
    for I in 1 .. 4 loop
        T.Increment;
    end loop;
    T.Reset;
    for I in 1 .. 4 loop
        T.Increment;
    end loop;
end;
