with Ada.Text_IO; use Ada.Text_IO;

procedure io_ops is
    F : File_Type;
    File_Name : constant string := "simple.txt";
    Dummy : constant string := "dummy";
begin
    Create(F, Out_File, File_Name);
    for i in 1 .. 3 loop
        Put_Line(F, "Hello nice world #" & i'image);
    end loop;
    close(F);

    Open(F, In_File, File_Name);
    while not End_Of_File(F) loop
        Put_Line(Get_Line(F));
    end loop;
    Close(F);

    Create(F, Out_File, Dummy);
    Delete(F);
    Open(F, In_File, Dummy);
    Close(F);
exception
    when Name_Error =>
        Put_Line("File " & Dummy & " doesn't exist");
    when others =>
        Put_Line("Error while processing input file.");
end;
