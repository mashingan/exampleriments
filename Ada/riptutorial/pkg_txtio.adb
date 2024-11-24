with Ada.Text_IO;

procedure pkg_txtio is
    use Ada.Text_IO;
    S : String := "Hello";
begin
    Put_Line("Hello");
    Put_Line(Standard_Output, "Hello");
    Put_Line(Standard_error, "Hello error");
    Put_Line(S & " World");
end;
