with Ada.Text_IO; use Ada.Text_IO;
with Increment_by;

procedure show_increment is
    A, B, C : Integer;
    procedure report(A, B, C: Integer) is
    begin
        Put_Line(
            "Increment of" & A'image &
            " with" & B'image &
            " is" & c'image);
    end report;
begin
    C := Increment_by;
    Put_Line("Using default for increment_by is" & C'image);

    A := 10;
    B := 3;
    C := Increment_by(A, B);
    report (A, B, C);
    A := 20;
    B := 5;
    C := increment_by(
        I => A,
        Step => B);
    report (A, B, C);
end show_increment;
