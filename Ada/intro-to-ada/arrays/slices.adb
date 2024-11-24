with Ada.Text_IO; use Ada.Text_IO;

procedure slices is
    Buf : String := "Hello ...";
    Full_Name : string := "John smith";
begin
    Buf (Buf'last - 2 .. Buf'last) := "Bob";
    Put_Line(Buf);
    Put("Hi, " & Full_Name(Full_Name'first .. Full_Name'first+3) & "!");
end;
