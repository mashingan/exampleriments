with Ada.Text_IO; use Ada.Text_IO;

procedure In_Out_Params is
    procedure swap(A, B: in out integer) is
        Tmp: Integer;
    begin
        tmp := A;
        A := B;
        B := tmp;
    end swap;

    A : Integer := 12;
    B : Integer := 44;
begin
    Swap(A, B);
    Put_Line(A'image);
end In_Out_Params;
