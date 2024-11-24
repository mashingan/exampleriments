with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

package body MyUtils is
    package Scon renames Ada.Strings.Maps.Constants;
    package Sfix renames Ada.Strings.Fixed;

    function CapFirst (s: string) return string is
        capstr : string := sfix.translate(s(s'first .. s'first), scon.upper_case_map);
        rest : string := sfix.translate(s(s'first+1 .. s'last), scon.lower_case_map);
    begin
        return capstr & rest;
    end CapFirst;

    procedure Generic_Swap (X, Y : in out T) is
        Tmp : Constant T := X;
    begin
        X := Y;
        Y := Tmp;
    end Generic_Swap;
end MyUtils;
