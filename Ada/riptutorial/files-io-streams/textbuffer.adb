with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

procedure textbuffer is
    type Fruit is (Banana, Pear, Orange, Melon);
    package Fruit_IO is new Enumeration_IO(Fruit);
    Buffer : String (1 .. Fruit'Width);
begin
    for I in Fruit range Pear .. Fruit'Last loop
        Fruit_IO.Put(To => Buffer,
                     Item => I,
                     Set => Lower_case);
        Buffer (Buffer'First) := To_Upper(Buffer (Buffer'first));
        Put_Line(buffer);
    end loop;
end;
