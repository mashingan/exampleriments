with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;

procedure decimal_fixedpoint is
    type T3_D3 is delta 10.0 ** (-3) digits 3;
    type T6_D3 is delta 10.0 ** (-3) digits 6;
    type T6_D6 is delta 10.0 ** (-6) digits 6;

    A : T3_D3 := T3_D3'delta;
    B : T3_D3 := 0.5;
    C : T6_D6;

    D : constant := 2.0 ** (-31);
    type TQ31 is delta D range -1.0 .. 1.0 - D;
    type TQ31v2 is delta 2.0 ** (-31) range -1.0 .. 1.0 - 2.0 ** (-31);

    type T_Inv_Trig is delta 2.0 ** (-15) * Pi range -Pi / 2.0 .. Pi / 2.0;

    A1, B1, R : TQ31;

    type Angle is delta 1.0/3600.0 range  0.0 .. 3600.0 - 1.0/3600.0;
    for Angle'small use Angle'delta;

begin
    Put_Line("The delta   value of T3_D3 is " & T3_D3'image(T3_D3'delta));
    Put_Line("The minimum value of T3_D3 is " & T3_D3'image(T3_D3'first));
    Put_Line("The maximum value of T3_D3 is " & T3_D3'image(T3_D3'last));
    New_Line;
    Put_Line("The delta   value of T6_D3 is " & T6_D3'image(T6_D3'delta));
    Put_Line("The minimum value of T6_D3 is " & T6_D3'image(T6_D3'first));
    Put_Line("The maximum value of T6_D3 is " & T6_D3'image(T6_D3'last));
    New_Line;
    Put_Line("The value of A    is " & A'image);
    A := A * B;
    Put_Line("The value of A *B is " & A'image);
    A := T3_D3'delta;
    C := A * B;
    Put_Line("The value of A *B is " & C'image);
    New_Line;
    Put_Line("TQ31 requires" & Integer'image(TQ31'size) & " bits.");
    Put_Line("The delta   value of TQ31 is " & TQ31'image(TQ31'delta));
    Put_Line("The minimum value of TQ31 is " & TQ31'image(TQ31'first));
    Put_Line("The maximum value of TQ31 is " & TQ31'image(TQ31'last));
    New_Line;
    Put_Line("T_Inv_Trig requires" & Integer'image(T_Inv_Trig'size) & " bits.");
    Put_Line("The delta   value of T_Inv_Trig is " & T_Inv_Trig'image(T_Inv_Trig'delta));
    Put_Line("The minimum value of T_Inv_Trig is " & T_Inv_Trig'image(T_Inv_Trig'first));
    Put_Line("The maximum value of T_Inv_Trig is " & T_Inv_Trig'image(T_Inv_Trig'last));
    New_Line;
    A1 := 0.25;
    B1 := 0.50;
    R := A1 + B1;
    Put_Line("R is " & R'image);
    R := A1 - B1;
    Put_Line("R is " & R'image);
end;
