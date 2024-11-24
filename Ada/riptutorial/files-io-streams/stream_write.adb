with Ada.Streams.Stream_IO;

procedure stream_write is
    type Fruit is (Banana, Orange, Pear, Melon);
    type Color_Value is range 0 .. 255;
    type Color is record
        R, G, B: Color_Value;
    end record;

    Fruit_Colors: constant array (Fruit) of Color := (
        Banana  => Color'(R => 243, G => 227, B => 18),
        Orange  => Color'(R => 251, G => 130, B => 51),
        Melon   => Color'(R => 175, G => 255, B => 0),
        Pear    => Color'(R => 158, G => 181, B => 94));

    use Ada.Streams.Stream_IO;

    F : File_Type;

begin
    Create (F, Name => "stream.bin");
    for C in Fruit_Colors'Range loop
        Fruit'Write (Stream (F), C);
        Color'Write (Stream (F), Fruit_Colors (C));
    end loop;
    Close (F);
end stream_write;
