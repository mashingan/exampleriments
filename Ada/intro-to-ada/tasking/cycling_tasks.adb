with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with aux_pkg;

procedure cycling_tasks is
    package aux renames aux_pkg;

    task t;

    task body t is
        cycle : constant Time_span := milliseconds (1000);
        next : time := Aux.get_start_time + cycle;
    begin
        for i in 1 .. 5 loop
            delay until next;

            aux.Show_Elapsed_Time;
            aux.Compute_Intensive;

            next := next + cycle;

            Put_Line("Cycle # " & I'image);
        end loop;
        Put_Line("Finished cycling");
    end t;
begin
    null;
end;
