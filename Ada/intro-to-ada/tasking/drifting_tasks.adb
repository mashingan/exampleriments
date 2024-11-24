with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with aux_pkg;

procedure drifting_tasks is
    package aux renames aux_pkg;

    task T;

    task body T is
    begin
        for I in 1 .. 5 loop
            delay 1.0;

            Aux.Show_Elapsed_Time;
            Aux.Compute_Intensive;

            Put_Line("Cycle #" & I'image);
        end loop;
        Put_Line("Finished time-drifting loop");
    end T;

begin
    null;
end;
