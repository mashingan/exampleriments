generic

type A_Type is private;

procedure Swap(Left, Right : in out A_Type) is
    Temp : A_Type := Left;
begin
    Left := Right;
    Right := Temp;
end Swap;
