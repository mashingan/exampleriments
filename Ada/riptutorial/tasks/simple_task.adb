with Ada.Text_IO; use Ada.Text_IO;

procedure simple_task is
    task My_Task;
    task body My_Task is
    begin
        Put_Line("Hello from my task!");
    end My_Task;
begin
    Put_Line("Hello from main");
end;
