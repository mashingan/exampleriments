with Ada.Text_IO; use Ada.Text_IO;

procedure pkgenum is
    type Fruit is (Banana, Pear, Orange, Melon);
    package Fruit_IO is new Enumeration_IO(Fruit); use Fruit_IO;
begin
    for I in Fruit loop
        Put (I);
        New_Line;
    end loop;
end;
