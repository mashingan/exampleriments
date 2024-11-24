-- compile it:
-- gnatmake -O2 Day_09_smoke_basin.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_09_smoke_basin.exe
-- or running build-run.bat
-- to clean:
-- adaclean
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Day_09_smoke_basin is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;

	subtype Elevation is Integer range 0 .. 9;
	package Elevation_Vectors is new Ada.Containers.Vectors
		(Index_Type => Natural, Element_Type => Elevation);
	package ev renames Elevation_Vectors;

	package Map_Vectors is new Ada.Containers.Vectors
		(Index_Type => Natural, Element_Type => ev.Vector, "=" => ev."=");
	package mv renames Map_Vectors;

	Elevation_Map : mv.Vector;

	procedure Print_Elevation(p: in ev.Cursor) is
	begin
		tio.Put(ev.Element(p)'image);
	end Print_Elevation;
	procedure Print_Contour(p: in mv.Cursor) is
	begin
		mv.Element(p).Iterate(Print_Elevation'access);
		tio.New_Line;
	end Print_Contour;
	procedure Cartography is
	begin
		Elevation_Map.Iterate(Print_Contour'access);
		tio.New_Line;
	end Cartography;

	function Read_Contour return ev.Vector is
		chr: Character;
		Contour: ev.Vector;
		length: Natural;
		line: String(1..32);
		cbuf: String(1..1);
		n: Integer;
		last: Positive;
	begin
		Contour.Reserve_Capacity(128);
		tio.Get_Line(line, length);
		for I in line'first .. length loop
			chr := line(I);
			if chr >= '0' and chr <= '9' then
				cbuf := "" & chr;
				nio.Get(cbuf, n, last);
				Contour.Append(Elevation(n));
			end if;
		end loop;
		return Contour;
	end Read_Contour;

	procedure Read_Map is
	begin
		Elevation_Map.Reserve_Capacity(32);
		loop
			begin
				Elevation_Map.append(Read_Contour);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
		end loop;
	end Read_Map;

	function All_Risk_levels return Integer is
		Sum_Risk_levels: Integer := 0;
		Contour: ev.Vector;
		Lower_elevation: Boolean := false;
	begin
		for I in Elevation_Map.First_Index .. Elevation_Map.Last_Index loop
			Contour := Elevation_Map(I);
			for J in Contour.First_Index .. Contour.Last_Index loop
				Lower_elevation := true;
				if Lower_elevation and J-1 >= Contour.First_Index then
					if Contour(J-1) < Contour(J) then Lower_elevation := false; end if;
				end if;

				if Lower_elevation and J+1 <= Contour.Last_Index then
					if Contour(J+1) < Contour(J) then Lower_elevation := false; end if;
				end if;

				if Lower_elevation and I-1 >= Elevation_Map.First_Index then
					if Elevation_Map(I-1)(J) < Contour(J) then Lower_elevation := false; end if;
				end if;

				if Lower_elevation and I+1 <= Elevation_Map.Last_Index then
					if Elevation_Map(I+1)(J) < Contour(J) then Lower_elevation := false; end if;
				end if;

				if Lower_elevation then
					Sum_Risk_levels := Sum_Risk_levels + Contour(J) + 1;
				end if;
			end loop;
		end loop;
		return Sum_Risk_levels;
	end All_Risk_levels;
begin
	Read_Map;
	tio.Put_Line("== current Cartography ==");
	Cartography;
	tio.Put("Sum_Risk_levels:" & All_Risk_levels'image);
end Day_09_smoke_basin;
