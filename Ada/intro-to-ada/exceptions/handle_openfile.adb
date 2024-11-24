with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure handle_openfile is
    File : File_Type;
begin
    Open(File, In_File, "input.txt");
exception
    when Name_error =>
        Put("Cannot open input file");
end handle_openfile;
