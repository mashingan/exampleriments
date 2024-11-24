-- compile it: (small)
-- gnatmake -O2 day_17_trick_shot.adb -bargs -largs -s
-- (normal)
-- gnatmake day_17_trick_shot.adb
-- run in cmd (not powershell)
-- type input.txt | day_17_trick_shot.exe
-- or simply run:
-- build-run.bat
-- to clean folder: adaclean
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure day_17_trick_shot is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	package fix renames Ada.Strings.Fixed;

	type Pos is record
		x, y: Integer := 0;
	end record;

	function To_String(p: in Pos) return String is
		('(' & p.x'image & ',' & p.y'image & ')');

	Ship : Pos;
	Move_point : Pos;

	Left : Pos;
	Right : Pos;

	procedure Read_Target_Area is
		line: String(1 .. 50);
		length : Natural;
		pos : Integer;
		chr : Character;
		xmin, xmax, ymin, ymax, vmin, vmax : Integer;
		dummy: String(1..2);
		last: Positive;
	begin
		tio.Get_Line(line, length);
		tio.Put_Line("Read line: " & line(1..length) & " with length:" & length'image);
		last := 1;
		for I in 1 .. 2 loop
			pos := fix.Index(line(last..length), "=");
			chr := line(pos-1);
			nio.Get(line(pos+1..length), vmin, last);
			--tio.Put_Line("Last:" & Last'image & ", line(last+1): " & line(last+1));
			nio.Get(line(last+3..length), vmax, last);
			--tio.Put_Line("Last:" & Last'image & ", line(last+1): " & line(last+1));
			last := last + 1;
			if chr = 'x' then
				xmin := vmin;
				xmax := vmax;
			else
				ymin := vmin;
				ymax := vmax;
			end if;
		end loop;
		Left := (Integer'Min(xmin, xmax), Integer'Min(ymin, ymax));
		Right := (Integer'Max(xmin, xmax), Integer'Max(ymin, ymax));
	end Read_Target_Area;

	function Retrieve_Steps_x(maxpos: in Integer) return Integer is
		count : Integer := 0;
		progress : Integer := abs(maxpos);
		negative : Boolean := maxpos < 0;
	begin
		loop
			count := count + 1;
			progress := progress - (count * (count - 1)) / 2;
			exit when progress < 0;
		end loop;
		if negative then
			count := -1 * count;
		end if;
		return count;
	end Retrieve_Steps_x;

	function Retrieve_Steps_y(minpos: In Integer) return Integer is
		count : Integer := 0;
		progress : Integer := abs(minpos);
		negative : Boolean := minpos < 0;
		height : Integer := 0;
	begin
		if negative then
			count := progress - 1;
		else
			count := progress;
		end if;
		return count;
	end Retrieve_Steps_y;

	procedure Trace_steps(Max_Step : Natural := 0) is
		count : Integer := Max_Step;
	begin
		tio.Put_Line("Left: " & To_String(Left) & ", Right: " & To_String(Right));
		loop
			tio.Put_Line("Speed:" & To_String(Ship) & " => Current:" & To_String(Move_point));
			if Max_Step = 0 then
				exit when Move_point.x > Right.x or Move_point.y < Right.y;
			else
				count := count - 1;
				if count = 0 then exit; end if;
			end if;
			Move_point.x := Move_point.x + ship.x;
			Move_point.y := Move_point.y + ship.y;
			if ship.x > 0 then
				ship.x := ship.x - 1;
			elsif ship.x < 0 then
				ship.x := ship.x + 1;
			end if;
			ship.y := ship.y - 1;
		end loop;
	end Trace_steps;

	progress : Integer := 0;
	count : Integer := 0;
begin
	ship.x := 6;
	ship.y := 9;
	Read_Target_Area;
	ship.x := Retrieve_Steps_x(Right.x);
	tio.Put_Line("ship.x:" & ship.x'image);
	count := 0;
	ship.y := Retrieve_Steps_y(Integer'Min(Left.y, Right.y));
	tio.Put_Line("ship.y:" & ship.y'image);
	Trace_steps;
end day_17_trick_shot;
