with Ada.Text_IO; use Ada.Text_IO;

procedure task_type is
    task type TT;

    task body TT is
    begin
        Put_Line("In task type TT");
    end TT;

    A_task : TT;

    task type TT2 is
        entry Start (N: Integer);
    end TT2;
    task body TT2 is
        task_n : integer;
    begin
        accept Start (N : integer) do
            Task_N := N;
        end start;
        Put_Line("In task TT2: " & task_n'image);
    end TT2;
begin
    Put_Line("In main");
    declare
        newtask : TT;
        My_tasks : array (1 .. 5) of TT2;
    begin
        for i in my_tasks'range loop
            My_tasks(i).start(i);
        end loop;
    end;
end;
