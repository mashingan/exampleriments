with Ada.Text_IO;

procedure Hello is
	package tio renames Ada.Text_IO;
begin
	tio.Put_Line("hello world Ada");
end Hello;
