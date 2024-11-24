with Ada.Text_IO; use Ada.Text_IO;

procedure generic_examples is

    generic
    type AnyType is private;
    InOutT : in out AnyType;
    procedure Set (E: AnyType);
    procedure Set (E: AnyType) is
    begin
        InOutT := E;
    end Set;

    --type AnyDiscreteType is (<>);
    --type AnyFloating is digits <>;

    --procedure Operator (X: InOutT);
    --procedure Operator (X: InOutT) is null;


    Main : Integer := 0;
    Current : Integer;

    procedure Set_Main is new Set(AnyType => Integer, InOutT => Main);

begin
    Current := 10;
    Set_Main (Current);
    Put_Line ("Value of main now is " & Main'image);
end;
