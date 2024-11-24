with Ada.Text_IO; use Ada.Text_IO;

procedure simple_tasks is
    task T;
    task T2;

    task body T is
    begin
        Put_Line("In Task T");
    end T;
    task body T2 is
    begin
        Put_Line("In Task T2");
    end T2;
begin
    Put_Line("In main");
end;
