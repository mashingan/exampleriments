with Ada.Text_IO; use Ada.Text_IO;

procedure hex_base is
    subtype Count is Integer range -1_000_000 .. 1_000_000;

    package Count_IO is new Integer_IO(Count);
    X: Count;

begin
    Count_IO.Default_Width := 1;
    Count_IO.Default_Base := 16;

    X := Count'First;
    while X < Count'Last loop
        Count_IO.Put(X);
        New_Line;
        X := X + 500_000;
    end loop;
end hex_base;
