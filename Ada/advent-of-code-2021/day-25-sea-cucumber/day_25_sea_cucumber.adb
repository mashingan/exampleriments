-- compile it: (small)
-- gnatmake -O2 day_25_sea_cucumber.adb -bargs -largs -s
-- (normal)
-- gnatmake day_25_sea_cucumber.adb
-- run in cmd (not powershell)
-- type input.txt | day_25_sea_cucumber.exe
-- or simply run:
-- build-run.bat
-- to clean folder: adaclean

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure day_25_sea_cucumber is
	package tio renames Ada.Text_IO;

	type Map_Herd is ('.', 'v', '>');

	package Herd_Line_Vector is new Ada.Containers.Vectors
		(Element_Type => Map_Herd, Index_Type => Natural);
	package hlv renames Herd_Line_Vector;

	package Herds_Vector is new Ada.Containers.Vectors
		(Element_Type => hlv.Vector, Index_Type => Natural,
		"=" => hlv."=");
	package hv renames Herds_Vector;

	function To_String(v: hlv.Vector) return String is
		res : String(1 .. Integer(v.Length));
	begin
		for I in v.First_Index..v.Last_Index loop
			res(I+1) := Character'value(Map_Herd'image(v(I)));
		end loop;
		return res;
	end To_String;

	Sea_cucumber_herds : hv.Vector := hv.Empty_Vector;

	procedure Read_herds is
		line: string(1..40);
		length: Natural := 0;
		vec : hlv.Vector := hlv.Empty_Vector;
	begin
		loop
			begin
				tio.Get_Line(line, length);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			exit when length = 0;
			for C of line(1..length) loop
				vec.Append(Map_Herd'Value(C'image));
			end loop;
			Sea_cucumber_herds.Append (vec);
			vec.Clear;
		end loop;
	end Read_herds;

	procedure check_herds(prompt: in string := "";
		herds : in hv.Vector := Sea_cucumber_herds) is
	begin
		if prompt /= "" then
			tio.Put_Line (prompt);
		end if;
		for vec of herds loop
			for H of vec loop
				tio.Put(Character'value(H'image));
			end loop;
			tio.New_Line;
		end loop;
	end check_herds;

	function step_left return Boolean is
		vec, newvec : hlv.Vector := hlv.Empty_Vector;
		moved : Boolean := false;
	begin
		for V in Sea_cucumber_herds.First_Index .. Sea_cucumber_herds.Last_Index loop
			vec := Sea_cucumber_herds(V);
			newvec := vec.Copy(vec.Length);
			--tio.Put_Line ("prev vec : " & To_String (vec));
			for H in vec.First_Index .. vec.Last_Index loop
				if H = vec.Last_Index then
					if vec(H) = '>' and vec(vec.First_Index) = '.' then
						moved := true;
						newvec(H) := '.';
						newvec(vec.First_Index) := '>';
					end if;
				else
					if vec(H) = '>' and vec(H+1) = '.' then
						moved := true;
						newvec(H+1) := '>';
						newvec(H) := '.';
					end if;
				end if;
			end loop;
			--tio.Put_Line ("after vec: " & To_String (newvec));
			Sea_cucumber_herds(V) := newvec;
		end loop;
		return moved;
	end step_left;

	function step_down return Boolean is
		moved : Boolean := false;
		vec, nextvec : hlv.Vector := hlv.Empty_Vector;
		newherds : hv.Vector := Sea_cucumber_herds.Copy (Sea_cucumber_herds.Length);
		got_last_index : Boolean := false;
	begin
		for V in Sea_cucumber_herds.First_Index..Sea_cucumber_herds.Last_Index loop
			vec := Sea_cucumber_herds(V);
			if V = Sea_cucumber_herds.Last_Index then
				got_last_index := true;
				nextvec := Sea_cucumber_herds.First_Element;
			else
				nextvec := Sea_cucumber_herds(V+1);
			end if;
			for H in vec.First_Index..vec.Last_Index loop
				if vec(H) = 'v' and nextvec(H) = '.' then
					moved := true;
					newherds(V)(H) := '.';
					if got_last_index then
						newherds(newherds.First_Index)(H) := 'v';
					else
						newherds(V+1)(H) := 'v';
					end if;
				end if;
			end loop;
		end loop;
		Sea_cucumber_herds := newherds;
		return moved;
	end step_down;

	function step_left_down return Boolean is
		moved_left, moved_down : Boolean := false;
	begin
		moved_left := step_left;
		moved_down := step_down;
		return moved_left or moved_down;
	end step_left_down;

	count : Natural := 0;
begin
	Read_herds;
	check_herds("Initial state:");
	loop
		count := count + 1;
		exit when not step_left_down;
	end loop;
	check_herds ("Last no movements:");
	tio.Put_Line ("No movement after" & count'image & " step(s)");
end day_25_sea_cucumber;
