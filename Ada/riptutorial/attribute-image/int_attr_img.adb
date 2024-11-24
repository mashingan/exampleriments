with ada.text_io;

procedure int_attr_img is
    type Some_Integer is range -42 .. 42;
    X: integer := 17;
    Y: Some_Integer := 18;
begin
    ada.text_io.put_line (integer'image (X));
    ada.text_io.put_line (Y'image);
end int_attr_img;
