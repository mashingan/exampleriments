with Ada.Text_IO; use Ada.Text_IO;

procedure delays is
    task T;

    task body T is
    begin
        for I in 1 .. 5 loop
            Put_Line("Hello from task T " & I'image & " with delay 0.5s");
            delay 0.5;
            --    ^ wait for 500 ms
        end loop;
    end T;
begin
    delay 1.5;
    Put_Line("hello from main after 1.5 seconds");
end;
