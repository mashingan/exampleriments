with Ada.Text_IO; use Ada.Text_IO;

procedure two_loops is
    task My_Task;
    task body My_Task is
    begin
        for I in 1..4 loop
            Put_Line("Hello from my task " & I'image);
        end loop;
    end;
begin
    for I in 1..4 loop
        Put_Line("Hello from my main " & I'image);
    end loop;
end;
