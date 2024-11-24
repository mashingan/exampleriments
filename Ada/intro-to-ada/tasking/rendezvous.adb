with Ada.Text_IO; use Ada.Text_IO;

procedure rendezvous is
    task T is
        entry Start;
    end T;

    task body T is
    begin
        accept Start; -- wait for somebody calling the entry
        Put_Line("In T");
    end T;

begin
    Put_Line("In main");
    T.Start;
end;
