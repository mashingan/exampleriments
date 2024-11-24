with Ada.Text_IO; use Ada.Text_IO;

procedure space_prints is
    subtype Count is Integer range -1_000_000 .. 1_000_000;

    package Count_IO is new Integer_IO(Count);
    X: Count;

begin
    Count_IO.Default_Width := 12;

    X := Count'First;
    while X < Count'Last loop
        Count_IO.Put(X);
        Count_IO.Put(X + 1);
        New_Line;

        X := X + 500_000;
    end loop;
end space_prints;
