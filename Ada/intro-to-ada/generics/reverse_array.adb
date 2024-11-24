with Ada.Text_IO; use Ada.Text_IO;
with myutils; use myutils;

procedure Reverse_Array is

    generic
    type T is private;
    type Index is range <>;
    type Array_T is array (Index range <>) of T;
    procedure Generic_Reverse_Array (X: in out Array_T);

    procedure Generic_Reverse_Array (X: in out Array_T) is
        procedure T_Swap is new Generic_Swap (T => T);
    begin
        for I in X'first .. (X'last + X'first) / 2 loop
            declare
                --Tmp : T;
                X_left : T renames X(I);
                X_right : T renames X (X'last + X'first - I);
            begin
                --Tmp := X_left;
                T_Swap(X_left, X_right);
            end;
        end loop;
    end Generic_Reverse_Array;

    type Color is (Black, Red, Green, Blue, White);
    type Color_Array is array (Integer range <>) of Color;

    procedure Reverse_Color_Array is new Generic_Reverse_Array(
    T => Color, Index => Integer, Array_T => Color_Array);

    My_Colors : Color_Array(1 .. 5) := (Black, Red, Green, Blue, White);

begin
    for C of My_Colors loop
        Put_Line("My_Colors: " & CapFirst(C'image));
    end loop;

    New_Line;
    Put_Line("Reversing My colors...");
    New_Line;
    Reverse_Color_Array (My_Colors);

    for C of My_Colors loop
        Put_Line("My_Colors: " & CapFirst(C'image));
    end loop;
end reverse_array;
