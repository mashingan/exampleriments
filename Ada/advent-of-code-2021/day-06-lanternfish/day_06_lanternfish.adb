-- compile it:
-- gnatmake -O2 Day_06_lanternfish.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_06_lanternfish.exe
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;

procedure Day_06_lanternfish is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	package lat renames Ada.Characters.Latin_1;

	type New_Spawn_Lifetime is range 0 .. 8;
	type Next_Cycle is range 0 .. 6;
	Target_Day : constant Integer := 80;

	package Integer_Vectors is new Ada.Containers.Vectors
		(Index_Type => Natural, Element_Type => Integer);
	package iv renames Integer_Vectors;

	School: iv.Vector;

	procedure Read_Init is
		num: Integer;
		chr: Character;
		last_line: Boolean;
	begin
		School.Reserve_Capacity(256);
		loop
			begin
				nio.Get(num);
				tio.Look_Ahead(chr, last_line);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			School.Append(num);
			exit when chr = lat.LF or last_line;
			tio.Get(chr);
		end loop;
	end Read_Init;

	procedure Fish_Cycle is
	begin
		for I in 1 .. Target_Day loop
			for J in School.First_Index .. School.Last_Index loop
				if School(J) = 0 then
					School(J) := 6;
					School.Append(8);
				else
					School(J) := School(J) - 1;
				end if;
			end loop;
		end loop;
	end Fish_Cycle;

	procedure Print_Elem(p: in iv.Cursor) is
	begin
		tio.Put(iv.Element(p)'image);
	end Print_Elem;

begin
	Read_Init;
	Fish_Cycle;
	tio.Put("Fish school after" & Target_Day'image & " days are" &
	School.Length'image);
end Day_06_lanternfish;
