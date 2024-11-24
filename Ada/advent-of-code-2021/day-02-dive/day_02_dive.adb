-- compile:
-- gnatmake -O2 Day_02_Dive.adb -bargs -largs -s
-- run in cmd (not in powershell)
-- type input.txt | Day_02_Dive.exe
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Day_02_Dive is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	use Ada.Strings.Fixed;
	depth: Natural := 0;
	pos: Integer := 0;
	final: Integer;
	last: Natural;
	line: string(1..20) := 20 * ' ';
	empty: string(1..20) := 20 * ' ';

	function get_forward(line: in String) return Integer is
		rest: String := line(line'first+8 .. line'last);
		result: Integer;
		last: Positive;
	begin
		nio.Get(rest, result, last);
		return result;
	end get_forward;

	function get_depth(line: in String; down: Boolean := true) return Natural is
		result: Natural;
		last: Positive;
		rest: String(1..20);
		displacement: Integer;
	begin
		displacement := if down then 5 else 3;
		move(line(line'first+displacement .. line'last), rest);
		nio.Get(rest, result, last);
		return result;
	end get_depth;
begin
	loop
		begin
			tio.Get_Line(line, last);
		exception
			when tio.Data_Error => exit;
			when tio.End_Error => exit;
		end;
		exit when line = empty;
		case line(1) is
			when 'f' => pos := pos + get_forward(line);
			when 'u' => depth := depth - get_depth(line, false);
			when 'd' => depth := depth + get_depth(line);
			when others => null;
		end case;
	end loop;
	final := depth * pos;
	tio.Put_Line("Depth: " & depth'image);
	tio.Put_Line("Pos: " & pos'image);
	tio.Put_Line("The final depth pos is " & final'image);
end Day_02_Dive;
