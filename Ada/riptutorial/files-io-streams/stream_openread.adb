with Ada.Streams.Stream_IO;

procedure stream_openread is
    type Fruit is (Banana, Orange, Pear, Melon);
    type Color_Value is range 0 .. 255;
    type Color is record
        R, G, B: Color_Value;
    end record;

    --Fruit_Colors: constant array (Fruit) of Color := (
    --    Banana  => Color'(R => 243, G => 227, B => 18),
    --    Orange  => Color'(R => 251, G => 130, B => 51),
    --    Melon   => Color'(R => 175, G => 255, B => 0),
    --    Pear    => Color'(R => 158, G => 181, B => 94));
    Fruit_Colors : array (Fruit) of Color;

    use Ada.Streams.Stream_IO;

    F : File_Type;
    X : Fruit;

begin
    Open (F, Mode => In_File, Name => "stream.bin");
    loop
        Fruit'Read (Stream (F), X);
        Color'Read (Stream (F), Fruit_Colors (X));
    end loop;
exception
    when End_Error =>
        Close (F);
        pragma Assert -- check data are the same
            (
                Fruit_Colors (Banana)  = Color'(R => 243, G => 227, B => 18) and
                Fruit_Colors (Orange)  = Color'(R => 251, G => 130, B => 51) and
                Fruit_Colors (Melon)   = Color'(R => 175, G => 255, B => 0) and
                Fruit_Colors (Pear)    = Color'(R => 158, G => 181, B => 94));
end stream_openread;
