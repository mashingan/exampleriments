with Ada.Text_IO; use Ada.Text_IO;

procedure tasks2_loops2 is
    task My_Task_1;
    task My_Task_2;
    task body My_Task_1 is
    begin
        for I in 1..4 loop
            Put_Line("Hello from my task_1 " & I'image);
        end loop;
    end;
    task body My_Task_2 is
    begin
        for I in 1..4 loop
            Put_Line("Hello from my task_2 " & I'image);
        end loop;
    end;
begin
    null;
end;
