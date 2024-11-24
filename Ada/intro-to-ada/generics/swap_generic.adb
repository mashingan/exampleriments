with Ada.Text_IO; use Ada.Text_IO;
with myutils; use myutils;

procedure swap_generic is
    type Color is (Black, Red, Green, Blue, White);

    procedure Swap_Colors is new Generic_Swap(T => Color);

    A, B, C : Color;

    procedure displayABC(A, B, C: Color) is
    begin
        Put_Line("The value of A is " & CapFirst(A'image));
        Put_Line("The value of B is " & CapFirst(B'image));
        Put_Line("The value of C is " & CapFirst(C'image));
    end displayABC;

begin
    A := Blue;
    B := Black;
    C := Red;
    displayABC(A, B, C);

    New_Line;
    Put_Line("Swapping A and C...");
    Swap_Colors(A, C);
    New_Line;

    displayABC(A, B, C);
end;
