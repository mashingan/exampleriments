with Ada.Text_IO; use Ada.Text_IO;
with pkg_sync;

procedure synchro is
    package ps renames pkg_sync;
    task T;
    task body T is
    begin
        for I in 1 .. 10 loop
            Put_Line("Hello in main package: " & I'image);
        end loop;
    end T;
begin
    null;
end;
