-- compile it:
-- gnatmake -O2 Day_01.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_01.exe
-- clean up: adaclean
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Day_01 is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	prev: Integer;
	next: Integer;
	increment: Integer := 0;
begin
	begin
		nio.Get(prev);
	exception
		when tio.End_Error =>
			tio.Put_Line("EOF");
			return;
		when tio.Data_Error =>
			tio.Put_Line("Invalid data, expect integer");
			return;
	end;
	loop
		begin
			nio.Get(next);
		exception
			when tio.End_Error => exit;
			when tio.Data_Error => exit;
		end;
		exit when next = 0;
		if next > prev then
			Increment := Increment + 1;
		end if;
		prev := next;
	end loop;
	tio.Put_Line("The increased from previous is " & Increment'image);
end Day_01;
