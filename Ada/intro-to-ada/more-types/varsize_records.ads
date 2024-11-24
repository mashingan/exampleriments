with compute_dummy; use compute_dummy;
package Varsize_Records is
    Max_Len : Constant Natural := Compute_Max_Len;

    type Items_Array is array (Positive range <>) of Integer;

    type Growable_Stack is record
        Items : Items_Array (1 .. Max_Len);
        Len : Natural;
    end record;

    type Growable_Stack_Discriminant (Max_Len : Natural) is record
        Items : Items_Array (1 .. Max_Len);
        Len : Natural := 0;
    end record;

    G : Growable_Stack;

end Varsize_Records;
