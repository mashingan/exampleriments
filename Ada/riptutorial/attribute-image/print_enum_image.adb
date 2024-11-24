with ada.text_io;

procedure print_enum_image is
    type Fruit is (Banana, Orange, Pear);
    X : Fruit := Orange;
begin
    Ada.Text_IO.Put_line (Fruit'Image (X));
end print_enum_image;
