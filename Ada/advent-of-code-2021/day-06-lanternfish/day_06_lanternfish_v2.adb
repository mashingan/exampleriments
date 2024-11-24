-- compile it:
-- gnatmake -O2 Day_06_lanternfish_v2.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_06_lanternfish_v2.exe
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;

procedure Day_06_lanternfish_v2 is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	package lat renames Ada.Characters.Latin_1;

	type New_Spawn_Lifetime is range 0 .. 8;
	Target_Day : constant Integer := 80;

	package Integer_Vectors is new Ada.Containers.Vectors
		(Index_Type => New_Spawn_Lifetime, Element_Type => Integer);
	package iv renames Integer_Vectors;

	School: iv.Vector := iv.To_Vector(9);

	procedure Read_Init is
		num: Integer;
		chr: Character;
		last_line: Boolean;
		lifetime: New_Spawn_Lifetime;
	begin
		for I in New_Spawn_Lifetime loop
			School(I) := 0;
		end loop;
		loop
			begin
				nio.Get(num);
				tio.Look_Ahead(chr, last_line);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			lifetime := New_Spawn_Lifetime(num);
			School(lifetime) := School(lifetime) + 1;
			exit when chr = lat.LF or last_line;
			tio.Get(chr);
		end loop;
	end Read_Init;

	procedure School_Cycle is
		procedure Print_Elem(p: in iv.Cursor) is
		begin
			tio.Put(iv.Element(p)'image);
		end Print_Elem;
	begin
		tio.Put("Cycles: ");
		School.Iterate(Print_Elem'access);
		tio.New_Line;
	end School_Cycle;

	procedure Fish_Cycle is
		spawner: Integer := 0;
		respawn_cycle: New_Spawn_Lifetime := New_Spawn_Lifetime'last - 2;
		last_cycle: New_Spawn_Lifetime := New_Spawn_Lifetime'last;
		first_cycle: New_Spawn_Lifetime := New_Spawn_Lifetime'first;
	begin
		--tio.Put("Beginning cycle:");
		--School_Cycle;
		for I in 1 .. Target_Day loop
			spawner := School(first_cycle);
			for J in first_cycle .. last_cycle-1 loop
				School.Swap(J, J+1);
			end loop;
			School(respawn_cycle) := School(respawn_cycle) + spawner;
			School(last_cycle) := spawner;
			tio.Put("Current cycle: day" & I'image & ' ');
			School_Cycle;
		end loop;
	end Fish_Cycle;

	Total_fishes: Integer := 0;
begin
	Read_Init;
	Fish_Cycle;
	for I in New_Spawn_Lifetime loop
		Total_fishes := Total_fishes + School(i);
	end loop;
	tio.Put("Fish school after" & Target_Day'image & " days are" &
	Total_fishes'image);
end Day_06_lanternfish_v2;
