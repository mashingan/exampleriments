with Ada.Text_IO; use Ada.Text_IO;

procedure multiprint_oneline is
    type Fruit is (Banana, Orange, Pear);
    subtype Count is Integer range -1_000_000 .. 1_000_000;

    package Fruit_IO is new Enumeration_IO(Fruit);
    package Count_IO is new Integer_IO(Count);

    Inventory : constant array (Fruit) of Count := (
        Banana  => +27_420,
        Orange  => +140_600,
        Pear    => -10_000);

begin
    Fruit_IO.Default_Width := 12;

    for F in Inventory'Range loop
        Fruit_IO.Put(F);
        Put(" | ");
        Count_IO.Put (Inventory (F));
        New_Line;
    end loop;
end multiprint_oneline;
