with Ada.Text_IO; use Ada.Text_IO;

package body pkg_sync is
    task body T is
    begin
        for I in 1 .. 10 loop
            Put_Line("Hello " & I'image);
        end loop;
    end T;
end;
