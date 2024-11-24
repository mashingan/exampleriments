with Ada.Text_IO,
Ada.Integer_Text_IO;

procedure blockexamples is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	function Get return Integer is
		Data : Integer := 0;
		Try_Limit : Constant := 3;
		Try_Count : Natural := 0;
	begin
		Input_Loop:
		loop
			Try_Count := Try_Count + 1;
			Try_block:
			declare
				Get_Last : Positive;
			begin
				nio.Get(tio.Get_Line, Data, Get_Last);
				exit Input_Loop;
			exception
				when tio.Data_Error =>
					tio.Put_Line("current try " & Try_Count'image);
					tio.Put_Line("try limit " & Try_Limit'image);
					if Try_Count > Try_Limit then
						tio.Put_Line("Too many tries");
						exit Input_Loop;
					end if;
			end Try_block;
		end loop Input_Loop;
		return Data;
	end Get;
begin
	tio.Put("The data got is " & Get'image);
end blockexamples;
