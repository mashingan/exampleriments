-- compile it:
-- gnatmake -O2 Day_05_hydrothermal_venture.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_05_hydrothermal_venture.exe
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Day_05_hydrothermal_venture is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	package fix renames Ada.Strings.Fixed;
	package hdl renames Ada.Characters.Handling;
	package sub renames Ada.Strings.Unbounded;

	use sub;

	Overlap: Integer := 0;
	type MapWidth is range 0..9;

	type Point is record
		x: MapWidth := 0;
		y: MapWidth := 0;
	end record;

	function To_String(p: in Point) return String is
		NumStr: String(1..1);
		Ub: sub.Unbounded_String;
	begin
		nio.Put(NumStr, Integer(p.x));
		Ub := '(' & To_Unbounded_String(NumStr) & ',';
		nio.Put(NumStr, Integer(p.y));
		Ub := Ub & NumStr & ')';
		return sub.To_String(Ub);
	end To_String;

	type Line is record
		p1: Point;
		P2: Point;
	end record;

	function To_String(ln: in Line) return String is
		(To_String(ln.p1) & " -> " & To_String(ln.p2));

	Map: array(MapWidth, MapWidth) of Integer;

	procedure Draw_Map is
		empty: string(1..2) := " .";
		Str: string(1..2);
	begin
		for I in MapWidth loop
			for J in MapWidth loop
				if Map(I, J) = 0 then
					tio.Put(empty);
				else
					nio.Put(Str, Map(I, J));
					tio.Put(Str);
				end if;
			end loop;
			tio.New_Line;
		end loop;
	end Draw_Map;

	task Init_Map;
	task body Init_Map is
	begin
		for I in MapWidth loop
			for J in MapWidth loop
				Map(I, J) := 0;
			end loop;
		end loop;
	end Init_Map;

	procedure Read_XY(x, y: out MapWidth) is
		char: Character;
		xx, yy: Integer;
	begin
		nio.Get(xx); tio.Get(char); nio.Get(yy);
		x := MapWidth(xx);
		y := MapWidth(yy);
	end Read_XY;

	procedure Read_Coord(p1, p2: out Point) is
		x, y: MapWidth;
		arrow: string(1..4);
	begin
		Read_XY(x, y);
		p1 := (x, y);
		tio.Get(arrow);
		Read_XY(x, y);
		p2 := (x, y);
	end Read_Coord;

	package Line_Queue_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
		(Element_Type => Line);
	package Line_Queue is new Ada.Containers.Unbounded_Synchronized_Queues
		(Queue_Interfaces => Line_Queue_Interface);
	Pipe_line: Line_Queue.Queue;

	task Read_Line;
	task body Read_Line is
		p1, p2: Point;
	begin
		loop exit when Init_Map'terminated; end loop;
		loop
			begin
				Read_Coord(p1, p2);
				Pipe_line.Enqueue(New_item => (p1, p2));
				--tio.Put_Line(To_String(p1) & " -> " & To_String(p2));
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
		end loop;
	end Read_Line;

	task Mark_Line;
	task body Mark_Line is
		Line_to_mark: Line;
		xlow, xhigh: MapWidth;
		ylow, yhigh: MapWidth;
	begin
		loop exit when Init_Map'terminated; end loop;
		loop select
			Pipe_line.Dequeue(Element => Line_to_mark);
			--tio.Put_Line(To_String(Line_to_mark));
			if (Line_to_mark.p1.x = Line_to_mark.p2.x) or (Line_to_mark.p1.y = Line_to_mark.p2.y) then
				xhigh := MapWidth'Max(Line_to_mark.p1.x, Line_to_mark.p2.x);
				xlow := MapWidth'Min(Line_to_mark.p1.x, Line_to_mark.p2.x);
				yhigh := MapWidth'Max(Line_to_mark.p1.y, Line_to_mark.p2.y);
				ylow := MapWidth'Min(Line_to_mark.p1.y, Line_to_mark.p2.y);
				for I in ylow .. yhigh loop
					for J in xlow .. xhigh loop
						Map(I, J) := Map(I, J) + 1;
					end loop;
				end loop;
			end if;
			else exit;
		end select;
		end loop;
	end Mark_Line;


	Total_overlap : Integer := 0;
begin
	--tio.Put_Line("Read_Line terminated? " & Boolean'image(Read_Line'terminated));
	--loop exit when Read_Line'terminated; end loop;
	loop exit when Mark_Line'terminated; end loop;
	Draw_Map;
	for I in MapWidth loop
		for J in MapWidth loop
			if Map(I, J) > 1 then Total_overlap := Total_overlap + 1; end if;
		end loop;
	end loop;
	tio.Put_Line("Total overlap:" & Total_overlap'image);
end Day_05_hydrothermal_venture;
