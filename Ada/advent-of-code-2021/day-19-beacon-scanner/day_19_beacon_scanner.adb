with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

procedure day_19_beacon_scanner is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;

	generic
	type Value_Type is private;
	package Coords is
		type Coord is record
			x, y, z: Value_Type;
		end record;
	end Coords;

	type Orientation is (Unknown, Up, Down);
	package Coord_Pkg is new Coords(Value_Type => Integer);

	function "="(c1, c2: in Coord_Pkg.Coord) return Boolean is
		(c1.x = c2.x and c1.y = c2.y and c1.z = c2.z);

	function To_String(c : in Coord_Pkg.Coord) return String is
		('(' & c.x'image & ',' & c.y'image & ',' & c.z'image & ')');

	package Orientation_Pkg is new Coords(Value_Type => Orientation);

	function To_String(c : in Orientation_Pkg.Coord) return String is
		('(' & c.x'image & ',' & c.y'image & ',' & c.z'image & ')');

	package Beacon_Vector is new Ada.Containers.Vectors
		(Element_Type => Coord_Pkg.Coord, Index_Type => Natural);
	package bv renames Beacon_Vector;

	type Scanner is record
		Id: Natural;
		position: Coord_pkg.Coord;
		orientation: Orientation_Pkg.Coord;
		beacons: bv.Vector := bv.Empty_Vector;
	end record;

	function "="(s1, s2: in Scanner) return Boolean is
		(s1.id = s2.id and s1.position = s2.position);

	package Scanner_Vector is new Ada.Containers.Vectors
		(Element_Type => Scanner, Index_Type => Natural);
	package sv renames Scanner_Vector;

	Scanners : sv.Vector := sv.Empty_Vector;

	function Read_Scanner_Id return Natural is
		package fix renames Ada.Strings.Fixed;
		scanner_line : String(1..30);
		length : Natural;
		num : Natural := 0;
		index : Integer := -1;
		last : Positive := 1;
	begin
		tio.Get_Line(scanner_line, length);
		index := fix.Index(scanner_line(1..length), "r");
		if index < 1 then return num; end if;
		nio.Get(scanner_line(index+2 .. length), num, last);
		return num;
	end Read_Scanner_Id;

	function Read_Scanner_Report return Boolean is
		line : String(1..15);
		length : Natural := 0;
		last : Positive := 1;
		device_scanner : Scanner;
		x, y, z : Integer := 0;
		ended : Boolean := false;
	begin
		device_scanner.Id := Read_Scanner_Id;
		device_scanner.beacons := bv.Empty_Vector;
		if device_scanner.Id = 0 then
			device_scanner.orientation := (Up, Up, Up);
		else
			device_scanner.orientation := (Unknown, Unknown, Unknown);
		end if;
		loop
			begin
				tio.Get_Line(line, length);
				exception
					when tio.End_Error =>
						ended := true;
						exit;
				end;
			exit when length = 0;
			nio.Get(line(last .. length), x, last);
			last := last + 2;
			nio.Get(line(last .. length), y, last);
			last := last + 2;
			nio.Get(line(last .. length), z, last);
			device_scanner.beacons.Append((x, y, z));
			last := 1;
		end loop;
		scanners.Append(device_scanner);
		return not ended;
	end Read_Scanner_Report;

	procedure Read_report is
	begin
		loop
			begin
				exit when not Read_Scanner_Report;
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
		end loop;
	end Read_report;

	procedure check_scanners is 
		scan_device : Scanner;
	begin
		for I in scanners.Iterate loop
			scan_device := scanners(I);
			tio.Put_Line("Scanner ID:" & scan_device.Id'image);
			tio.Put_Line(", Position: " & To_String (scan_device.Position));
			tio.Put_Line(", Orientation: " & To_String (scan_device.Orientation));
			for J in scan_device.beacons.Iterate loop
				tio.Put_Line (To_String(scan_device.beacons(J)));
			end loop;
		end loop;
	end check_scanners;

	function combinations(coord_in : in Coord_Pkg.Coord) return bv.Vector is
		res : bv.Vector := bv.Empty_Vector;
		subtype Pos_Index is Integer range 1..3;
		type Pos is array(Pos_Index) of Integer;

		posin : constant Pos := (coord_in.x, coord_in.y, coord_in.z);

		procedure comb is
		begin
			for I in Pos_Index loop
				for J in Pos_Index loop
					for K in Pos_Index loop
						if I /= J and J /= K and K /= I then
							res.Append ((posin(I), posin(J), posin(K)));
						end if;
					end loop;
				end loop;
			end loop;
		end comb;
	begin
		comb;
		return res;
	end combinations;

	first_comb : bv.Vector := bv.Empty_Vector;
begin
	Read_report;
	check_scanners;
	tio.Put_Line ("Combination of first beacon");
	first_comb := combinations(Scanners.First_Element.beacons.First_Element);
	for V in first_comb.First_Index .. first_comb.Last_Index loop
		tio.Put_Line (To_String(first_comb(V)));
	end loop;
end day_19_beacon_scanner;
