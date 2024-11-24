-- compile it:
-- gnatmake -O2 Day_07_treachery_whales.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_07_treachery_whales.exe
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;

procedure Day_07_treachery_whales is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	package lat renames Ada.Characters.Latin_1;

	package Integer_Vectors is new Ada.Containers.Vectors
		(Index_Type => Natural, Element_Type => Integer);
	package iv renames Integer_Vectors;

	Crabs: iv.Vector;

	procedure Read_Init(MinPos, MaxPos: out Integer) is
		num: Integer;
		chr: Character;
		last_line: Boolean;
	begin
		MinPos := Integer'last;
		MaxPos := Integer'first;
		Crabs.Reserve_Capacity(16);
		loop
			begin
				nio.Get(num);
				tio.Look_Ahead(chr, last_line);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			if num < MinPos then
				MinPos := num;
			end if;
			if num > MaxPos then
				MaxPos := num;
			end if;
			Crabs.Append(num);
			exit when chr = lat.LF or last_line;
			tio.Get(chr);
		end loop;
	end Read_Init;

	procedure Crabs_pos is
		procedure Print_Elem(p: in iv.Cursor) is
		begin
			tio.Put(iv.Element(p)'image);
		end Print_Elem;
	begin
		Crabs.Iterate(Print_Elem'access);
		tio.New_Line;
	end Crabs_pos;

	procedure Least_Fuel(MinPos, MaxPos: in Integer;
		outpost, fuel: out Integer) is
		Sum: Integer := 0;
	begin
		outpost := MinPos;
		fuel := Integer'last;
		for I in MinPos .. MaxPos loop
			for J in Crabs.First_Index .. Crabs.Last_Index loop
				Sum := Sum + abs(Crabs(J) - I);
			end loop;
			if Sum < fuel then
				outpost := I;
				fuel := Sum;
			end if;
			Sum := 0;
		end loop;
	end Least_Fuel;

	Min_Pos, Max_Pos, Outpost, Fuel: Integer;
begin
	Read_Init(Min_Pos, Max_Pos);
	Crabs_pos;
	Least_Fuel(Min_Pos, Max_Pos, Outpost, Fuel);
	tio.Put_Line("Min_Pos:" & Min_Pos'image & ", Max_Pos:" & Max_Pos'image);
	tio.Put_Line("Outpost:" & Outpost'image & ", Fuel:" & Fuel'image);
end Day_07_treachery_whales;
