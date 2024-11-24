with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;

procedure float_type is
    A : float := 2.5;

    procedure check_A(A: float) is
    begin
        Put_Line("The value of A is" & A'image);
    end check_A;

    -- precision floating types
    type T3 is digits 3;
    type T15 is digits 15;
    type T18 is digits 18;

    -- display custom float
    C1 : constant := 1.0e-4;
    Ac1 : T3 := 1.0 + c1;
    Bc1 : T18 := 1.0 + c1;

    -- ranged floating
    type T_Norm is new Float range -1.0 .. 1.0;
    At_norm : T_norm;

    -- custom range types
    type T6_Inv_Trig is digits 6 range -Pi/2.0 .. Pi/2.0;

begin
    check_A (A);

    -- floating operations
    A := abs(A - 4.5);
    check_A (A);
    A := A ** 2 + 1.0;
    check_A (A);

    -- see the precision floating
    Put_Line("T3 requires" & Integer'image(T3'size) & " bits");
    Put_Line("T15 requires" & Integer'image(T15'size) & " bits");
    Put_Line("T18 requires" & Integer'image(T18'size) & " bits");

    -- display custom float
    Put_Line("The value of Ac1 is" & T3'image(Ac1));
    Put_Line("The value of Bc1 is" & T18'image(Bc1));

    -- ranged floating
    At_norm := 1.0;
    Put_Line("The value of At_norm is" & T_Norm'Image(At_norm));

end;
