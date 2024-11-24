with Ada.Text_IO; use Ada.Text_IO;

procedure formal_subprograms is
    generic
    Description : String;
    type T is private;
    with function Comparison (X, Y: T) return Boolean;
    procedure Check (X, Y: T);

    procedure Check(X, Y: T) is
        Result : Boolean;
    begin
        Result := Comparison(X, Y);
        if Result then
            Put_Line("Comparison (" & Description &
            ") between argument is OK!");
        else
            Put_Line("Comparison (" & Description &
            ") between argument is not OK!");
        end if;
    end check;

    A, B : Integer;

    procedure Check_Is_Equal is new Check (
    Description => "equality",
    T => Integer,
    Comparison => Standard."=");

begin
    A := 0;
    B := 1;
    Check_Is_Equal(A, B);
end formal_subprograms;
