with Ada.Text_IO; use Ada.Text_IO;

package body aux_pkg is
    procedure Show_Elapsed_Time is
        Now_Time : Time;
        Elapsed_Time : Time_span;
    begin
        Now_time := clock;
        Elapsed_Time := Now_time - start_time;
        Put_Line(
            "Elapsed time: " &
            Duration'image(to_duration(Elapsed_Time)) &
            " seconds");
    end Show_Elapsed_Time;

    procedure Compute_Intensive is
    begin
        delay 0.5;
    end Compute_Intensive;
end;
