with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

procedure subtype_example is

    package StrConst renames Ada.Strings.Maps.Constants;
    package StrFix renames Ada.Strings.Fixed;

    type Days is (
        Monday, Tuesday, Wednesday, Thursday,
        Friday, Saturday, Sunday);

    subtype Weekend_Days is Days range Saturday .. Sunday;

    M : Days := Sunday;
    S : Weekend_Days := M;

    function CapFirst (item: String) return string is
    begin
        return StrFix.translate(item (item'first .. item'first), StrConst.Upper_case_map) &
               StrFix.translate(item (item'first + 1 .. item'last), StrConst.Lower_case_map);
    end;


begin
    for I in Days loop
        case I is
            when Weekend_Days =>
                Put_Line(CapFirst(I'image) & " week end!");
            when others =>
                Put_Line(CapFirst(I'Image) & " work day!");
        end case;
    end loop;
end;
