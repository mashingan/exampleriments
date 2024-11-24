with ada.text_io;

procedure float_attr_img is
    type Some_Float is digits 8 range 0.0 .. 10.0;
    X : float := 2.71;
    Y : Some_float := 2.71;
begin
    Ada.Text_IO.Put_line (float'image (X));
    Ada.Text_IO.Put_line (Y'image);
end float_attr_img;
