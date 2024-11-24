with ada.text_io;

procedure image_and_value is
    type Fruit is (Banana, Orange, Pear);
    X : Fruit := Orange;

begin
    Ada.Text_IO.Put_line (Boolean'Image
        (Fruit'Value (Fruit'Image (X)) = X
            and
        Fruit'Image (Fruit'Value("ORANGE")) = "ORANGE"));
end image_and_value;
