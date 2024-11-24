with Ada.Text_IO;

procedure file_write is
    use Ada.Text_IO;
    F : File_Type;
begin
    Create (F, Out_File, "file.txt");
    Put_Line (F, "This string will be written to the file file.txt");
    Close (F);
end;
