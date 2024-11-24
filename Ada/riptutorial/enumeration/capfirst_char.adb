with ada.text_io; use ada.text_io;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure capfirst_char is
    type Fruit is (Banana, Pear, Orange, Melon);
    function Case_Rule_For_Names(Item: String) return String is
    begin
        return Translate(Item (Item'First .. Item'First), Upper_case_map) &
        Translate(Item (Item'First + 1 .. Item'Last), Lower_case_map);
    end;
begin
    for I in Fruit loop
        Put(Case_Rule_For_Names (Fruit'Image(I)));
        New_Line;
    end loop;
end capfirst_char;
