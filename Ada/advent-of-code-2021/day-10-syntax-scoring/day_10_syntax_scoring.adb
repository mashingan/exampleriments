-- compile it:
-- gnatmake -O2 Day_10_syntax_scoring.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_10_syntax_scoring.exe
with Ada.Text_IO;
with Ada.Containers.Vectors;

with stacks;

procedure Day_10_syntax_scoring is
	package tio renames Ada.Text_IO;

	type Brackets is (
		'{', '<', '(', '[', '}', '>', ')', ']'
		);
	subtype Open_Brackets is Brackets range '{' .. '[';
	subtype Close_Brackets is Brackets range '}' .. ']';
	type Brackets_Array is array (1..128) of Brackets;
	type Brackets_Container is record
		brackets: Brackets_Array;
		length: Natural := 1;
	end record;

	function To_String(brc: in Brackets_Container) return String is
		res: String(1..128);
	begin
		for I in 1 .. brc.length loop
			res(I) := Character'Value(brc.brackets(I)'image);
		end loop;
		return res(1..brc.length);
	end To_String;

	package Char_Stacks is new Stacks
		(Element_Type => Close_Brackets, Max => 128);
	package cs renames Char_Stacks;

	package Bracket_Vector is new ada.Containers.Vectors
		(Index_Type => Natural, Element_Type => Brackets_Container);
	package bv renames Bracket_Vector;

	all_brackets : bv.Vector;

	procedure Read_Syntaxes is
		line: String(1..129);
		length: Natural;
		brc: Brackets_Container;
		count: Natural := 0;
		bra: Brackets;
	begin
		all_brackets.Reserve_Capacity(32);
		loop
			count := 1;
			begin
				tio.Get_Line(line, length);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			tio.Put_Line("Read line:" & line(1..length));
			for I in line'first .. length loop
				bra := Brackets'value(line(I)'image);
				brc.brackets(I) := bra;
			end loop;
			brc.length := length;
			all_brackets.append(brc);
		end loop;
	end Read_Syntaxes;

	function Close_Bracket_Value(b : Brackets) return Integer is
		val : array (Brackets) of Integer :=
			(')' => 3, ']' => 57, '}' => 1197, '>' => 25137, others => 0);
	begin
		return val(b);
	end Close_Bracket_Value;

	function Syntax_Checkers return Integer is
		syntax_stack : cs.Stack;
		container: Brackets_Container;
		bra: Brackets;
		cbr: Close_Brackets;
		Total : Integer := 0;
		Add : Integer;
	begin
		for I in all_brackets.First_Index .. all_brackets.Last_Index loop
			cs.Empty(syntax_stack);
			container := all_brackets(I);
			--tio.Put_Line("Got container:" & To_String(container));
			Add := 0;
			for I in container.brackets'first .. container.length loop
				bra := container.brackets(I);
				if bra in Close_Brackets then
					if cs.Is_Empty(syntax_stack) then
						Add := Close_Bracket_Value(bra);
						exit;
					end if;
					cbr := cs.Pop(syntax_stack);
					if cbr /= bra then
						Add := Close_Bracket_Value(bra);
						exit;
					end if;
				elsif bra in Open_Brackets then
					cbr := Brackets'val(Open_Brackets'pos(bra) +
						Open_Brackets'width+1);
					cs.Push(syntax_stack, cbr);
				end if;
			end loop;
			Total := Total + Add;
		end loop;
		return Total;
	end Syntax_Checkers;

	Total : integer := 0;
begin
	Read_Syntaxes;
	Total := Syntax_Checkers;
	tio.Put_Line("Total:" & Total'image);
end Day_10_syntax_scoring;
