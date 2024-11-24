with ada.text_io;

procedure attr_enum_image is
    type Fruit is (Banana, Orange, Pear);
    X : Fruit := Orange;
begin
    Ada.Text_IO.Put_line (X'Image);
    Ada.Text_IO.Put_line (Pear'Image);
end attr_enum_image;
