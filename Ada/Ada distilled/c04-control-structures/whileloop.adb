with Ada.Text_IO;
with Ada.Characters.Latin_1;

procedure whileloop is
	package tio renames Ada.Text_IO;
	The_File: tio.File_Type;
	As_Input : constant tio.File_Mode := tio.In_File;
	External_name: String := "./My.txt";
	The_Data : String (1..80);
	Line_Length : Natural;
	procedure Jo_Regelt is
	begin
		tio.Open(The_File, As_Input, External_name);
		Input_Routine:
		while not tio.End_Of_File(The_File) loop
			tio.Get_Line(The_File, The_Data, Line_Length);
			tio.Put_Line(The_Data(1..Line_Length));
			tio.Put_Line("string length: " & Line_Length'image);
		end loop Input_Routine;
		tio.Close(The_FIle);
	end Jo_Regelt;

	procedure Hello_by_input is
		ESC : Character renames Ada.Characters.Latin_1.Esc;
		Input : Character := Ada.Characters.Latin_1.Space;
		Index : Natural := 0;
		Hello : String (1..80) := (others => Input);
	begin
		tio.Put_Line("(enter any character, press ESC to exit)");
		tio.Get_Immediate(Input);
		while Input /= Esc loop
			tio.Put(Input);
			Index := Index + 1;
			Hello(index) := Input;
			Tio.Get_Immediate(Input);
		end loop;
		tio.New_Line;
		tio.Put_Line(Hello);
	end Hello_by_input;

	procedure Salaam_Ahly_Kham is
	begin
		tio.Open(The_File, As_Input, External_name);
		Controlled_Input:
		loop
			tio.Get_Line(The_File, The_Data, Line_Length);
			exit Controlled_Input
			when The_Data(1..2) = "##";
			tio.Put_Line(The_Data(1..Line_Length));
		end loop Controlled_Input;
		tio.Close(The_FIle);
	end Salaam_Ahly_Kham;
begin
	Jo_Regelt;
	Hello_by_input;
	Salaam_Ahly_Kham;
end whileloop;
