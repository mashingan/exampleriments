with Ada.Text_IO.Editing; use Ada.Text_IO;

procedure money_fixedpoint is
    Max_count : constant := 1_000_000;

    type Fruit is (Banana, Orange, Pear);
    subtype Count is Integer range -Max_count .. +Max_count;

    type Money is delta 0.001 digits 10;

    package Fruit_IO is new Enumeration_IO(Fruit);
    package Money_IO is new Editing.Decimal_Output
    (
        Money,
        Default_Currency => "CHF",
        Default_Separator => ''');

    Inventory : constant array (Fruit) of Count := (
        Banana  => +27_420,
        Orange  => +140_600,
        Pear    => -10_000);

    Price_List : constant array (Fruit) of Money := (
        Banana  => 0.07,
        Orange  => 0.085,
        Pear    => 0.21);

    Format : constant Editing.Picture := Editing.To_Picture ("<###BZ_ZZZ_ZZ9.99>");

begin
    Fruit_IO.Default_Width := 12;
    for F in Inventory'Range loop
        Fruit_IO.Put(F);
        Put(" | ");
        Money_IO.Put(
            Item => Inventory(F) * Price_List(F),
            Pic => Format);
        New_Line;
    end loop;
end money_fixedpoint;
