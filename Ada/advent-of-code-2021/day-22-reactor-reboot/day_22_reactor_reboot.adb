-- compile it: (small)
-- gnatmake -O2 day_22_reactor_reboot.adb -bargs -largs -s
-- (normal)
-- gnatmake day_22_reactor_reboot.adb
-- run in cmd (not powershell)
-- type input.txt | day_22_reactor_reboot.exe
-- or simply run:
-- build-run.bat
-- to clean folder: adaclean
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

procedure day_22_reactor_reboot is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	package fix renames Ada.Strings.Fixed;

	subtype Reactor_init_size is Integer range -51 .. 50;
	type Reactor_cubes is
		array (Reactor_init_size, Reactor_init_size, Reactor_init_size) of
		Boolean;

	reactor : Reactor_cubes := (others => (others => (others => false)));

	type Range_axis is record
		start_axis, end_axis: Reactor_init_size;
	end record;

	function "="(ra1, ra2: in Range_axis) return Boolean is
		(ra1.start_axis = ra2.start_axis and ra1.end_axis = ra2.end_axis);

	type Cuboids_Range is record
		on: Boolean;
		x, y, z: Range_axis;
	end record;

	function "="(cr1, cr2: in Cuboids_Range) return Boolean is
		(cr1.on = cr2.on and cr1.x = cr2.x and cr1.y = cr2.y and cr1.z = cr2.z);

	package Range_Vector is new Ada.Containers.Vectors
		(Element_Type => Cuboids_Range, Index_Type => Natural);
	package rv renames Range_Vector;

	Initialization_sequence : rv.Vector := rv.Empty_Vector;

	function read_range (s: in string; first: in Integer; last : in out Integer)
		return Range_axis is
		equalstr : String := "=";
		index : Integer := 0;
		ra : Range_axis;
		num : Integer;
	begin
		index := fix.Index(s(first .. s'last), equalstr);
		nio.Get(s(index+1 .. s'last), num, last);
		if num in Reactor_init_size then
			ra.start_axis := Reactor_init_size(num);
		else
			return (-51, -51);
		end if;
		last := last + 3;
		nio.Get(s(last .. s'last), num, last);
		last := last + 1;
		if num in Reactor_init_size then
			ra.end_axis := Reactor_init_size(num);
		else
			return (-51, -51);
		end if;
		return ra;
	end read_range;

	procedure assign_axis_range(c: Character; ra: in Range_axis;
		cr : in out Cuboids_Range) is
	begin
		if c = 'x' then
			cr.x := ra;
		elsif c = 'y' then
			cr.y := ra;
		else
			cr.z := ra;
		end if;
	end assign_axis_range;

	procedure process_sequence (s: in String) is
		index: Integer := 0;
		last: Positive := s'first;
		spacestr : String := " ";
		cr : Cuboids_Range;
		ra : Range_axis;
	begin
		cr.on := true;
		index := fix.Index(s, spacestr);
		if index not in s'range then return; end if;
		if s(s'first .. index-1) /= "on" then
			cr.on := false;
		end if;
		last := index;
		for I in 1..3 loop
			exit when last > s'last;
			ra := read_range (s, index+1, last);
			assign_axis_range (s(index+1), ra, cr);
			index := last;
		end loop;
		Initialization_sequence.Append (cr);
	end process_sequence;

	procedure Read_sequence is
		line: String(1..128);
		length: Natural := 0;
	begin
		loop
			begin
				tio.Get_Line(line, length);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			exit when length = 0;
			process_sequence (line(1..length));
		end loop;
	end Read_sequence;

	procedure apply_sequence(cr: in Cuboids_Range) is
		set_on : Boolean := cr.on;
	begin
		for I in cr.x.start_axis .. cr.x.end_axis loop
			for J in cr.y.start_axis .. cr.y.end_axis loop
				for K in cr.z.start_axis .. cr.z.end_axis loop
					reactor(I, J, K) := set_on;
				end loop;
			end loop;
		end loop;
	end apply_sequence;

	function Total_cubes_on return Integer is
		total : Integer := 0;
		dummy_unused : Integer := Reactor_init_size'first;
	begin
		for I in Reactor_init_size loop
			for J in Reactor_init_size loop
				for K in Reactor_init_size loop
					if I = dummy_unused or J = dummy_unused or K = dummy_unused then
						null;
					elsif reactor(I, J, K) then
						total := total + 1;
					end if;
				end loop;
			end loop;
		end loop;
		return total;
	end Total_cubes_on;

begin
	Read_sequence;
	for cr of Initialization_sequence loop
		tio.Put_Line (cr'image);
	end loop;
	for cr of Initialization_sequence loop
		apply_sequence (cr);
	end loop;
	tio.Put_Line("Total cubes on are" & Integer'Image(Total_cubes_on));
end day_22_reactor_reboot;
