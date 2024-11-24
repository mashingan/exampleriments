with Ada.Text_IO; use Ada.Text_IO;

procedure prepost_condition is
    pragma Assertion_Policy(Pre => check, Post => check);

    procedure DB_entry (name : string; age : natural)
        with Pre => Name'length > 0
    is
    begin
        null;
    end DB_entry;

    type Int_8 is range -2 ** 7 .. 2 ** 7 - 1;
    type Int_8_array is array (Integer range <>) of Int_8;

    function Square (A : Int_8) return Int_8
    is (A * A) with
        Pre => (Integer'size >= Int_8'size * 2 and
                Integer(A) * Integer(A) < Integer(Int_8'last)),
        Post => square'result > A;

    procedure Square(A : in out Int_8_array)
        with Post => (for all I in A'range => A(I) = A'Old(I) * A'Old(I))
    is
    begin
        for V of A loop
            V := square(V);
        end loop;
    end square;

    V : Int_8_array := (9, 10, 11);

    procedure fail_postcondition is
        V1 : Int_8_array := (1, 2);
    begin
        Square(V1); -- this will violate postcondition check
        for E of V loop
            Put_Line("Squared: " & E'image);
        end loop;
    end fail_postcondition;

    procedure fail_square_precond is
        V : Int_8;
    begin
        V := square(12);
        Put_Line("Square of 12 is " & V'image);
    end fail_square_precond;

begin
    DB_entry("John", 30);
    --DB_entry("", 21);       -- precondition will fail

    for E of V loop
        Put_Line("Original: " & E'image);
    end loop;
    New_Line;

    Square(V);
    for E of V loop
        Put_Line("Squared: " & E'image);
    end loop;
    fail_square_precond;
    fail_postcondition;
end prepost_condition;
