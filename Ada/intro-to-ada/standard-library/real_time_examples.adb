with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Execution_Time; use Ada.Execution_Time;
with Ada.Numerics.Generic_Elementary_Functions;

procedure real_time_examples is
    D : time_span := seconds (5);
    Next : Time := clock + d;
    procedure Compute_Intens is
    begin
        delay 0.5;
    end;
    procedure Real_Intense is
        package Funcs is new Ada.Numerics.Generic_Elementary_Functions (
        Float_Type => Long_Long_Float);
        X : Long_Long_Float;
        use Funcs;
    begin
        for I in 0 .. 1_000_000 loop
            X := Tan(Arctan(
                Tan(Arctan(
                    Tan(Arctan(
                        Tan(Arctan(
                            Tan(Arctan(
                                Tan(Arctan(0.577))))))))))));
        end loop;
    end;

    procedure benchmark_time is
        start, stop: Time;
        elapsed_time : TIme_span;
    begin
        start := clock;
        Compute_Intens;
        stop := clock;
        elapsed_Time := stop - start;
        Put_Line(
            "Elapsed time: " & duration'image(to_duration(elapsed_Time)) &
            " seconds.");
    end;

    generic
    with procedure Func;
    procedure f_benchmark;
    procedure f_benchmark is
        start, stop: cpu_time;
        elapsed_time : TIme_span;
    begin
        start := clock;
        Func;
        stop := clock;
        elapsed_Time := stop - start;
        Put_Line(
            "CPU time: " & duration'image(to_duration(elapsed_Time)) &
            " seconds.");
    end;
    procedure benchmark_cputime is new f_benchmark(Func => Compute_Intens);
    procedure real_benchmark is new f_benchmark(Func => Real_Intense);

begin
    Put("Let's wait " & duration'image(To_Duration(D)) & " seconds ... ");
    delay until next;
    Put_Line("Done!");
    benchmark_time;
    benchmark_cpuTime;
    real_benchmark;
end;
