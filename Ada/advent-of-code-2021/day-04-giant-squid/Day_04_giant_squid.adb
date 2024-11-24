-- compile it:
-- gnatmake -O2 Day_04_giant_squid.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_04_giant_squid.exe
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

procedure Day_04_giant_squid is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	package fix renames Ada.Strings.Fixed;
	package hdl renames Ada.Characters.Handling;
	package sub renames Ada.Strings.Unbounded;

	type BingoNum is record
		Num: Integer;
		Marked: Boolean := false;
	end record;

	function To_String(arg: BingoNum) return String is
		NumStr: String(1..10);
		Ub: sub.Unbounded_String;
	begin
		nio.Put(NumStr, arg.Num);
		Ub := sub.To_Unbounded_String(NumStr);
		if arg.Marked then
			sub.Append(ub, '^');
		end if;
		return sub.To_String(Ub);
	end To_String;

	type BoardWidth is range 1..5;
	type Board is array(BoardWidth, BoardWidth) of BingoNum;
	type DrawNums is array (Integer range <>) of Integer;
	Max_Draw : constant Integer := 100;
	Boards : array(1 .. 3) of Board;
	Draws : DrawNums := (1..Max_Draw => 0);

	DrawLen : Integer;

	function Get_Draws return Integer is
		Num: Integer;
		Length: Integer := 0;
		Comma: Character;
		Line_End: Boolean;
	begin
		loop
			begin
				nio.Get(Num);
				tio.Look_Ahead(Comma, Line_End);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			--tio.Put_Line("Num:" & num'image);
			Length := Length + 1;
			Draws(Length) := Num;
			exit when Length = Max_Draw or hdl.Is_Space(Comma) or hdl.Is_Control(Comma);
			tio.Get(Comma);
		end loop;
		return Length;
	end Get_Draws;

	procedure Get_Row(BoardNum, Row, DrawLength: in Integer) is
		--Char: Character;
		Num: Integer;
		Marked: Boolean := false;
	begin
		for I in BoardWidth loop
			begin
				nio.Get(Num);
				--tio.Get(Char);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			--exit when hdl.Is_Control(Char);
			--tio.Put_Line("Num:" & Num'image & " is marked? " & marked'image);
			Boards(BoardNum)(BoardWidth(Row), I) := (Num, Marked);
		end loop;
	end Get_Row;

	procedure Get_Board(BoardNum, DrawLen: in Integer) is
	begin
		for I in BoardWidth loop
			Get_Row(BoardNum, Integer(I), DrawLen);
		end loop;
	end Get_Board;

	procedure Get_Boards(DrawLen: in Integer) is
	begin
		for I in 1..3 loop
			Get_Board(I, DrawLen);
		end loop;
	end Get_Boards;

	task type BoardProcessor is
		entry Add_Board(BoardId: in Integer);
		entry Check_Board(DrawNum: in Integer; Win: out Integer);
		entry Done;
	end BoardProcessor;
	
	function Check_Row(The_Board: in Board; Row: in BoardWidth) return Boolean is
	begin
		for I in BoardWidth loop
			if not The_Board(Row, I).Marked then return false; end if;
		end loop;
		return true;
	end Check_Row;

	function Check_Column(The_Board: in Board; Col: in BoardWidth) return Boolean is
	begin
		for I in BoardWidth loop
			if not The_Board(I, Col).Marked then return false; end if;
		end loop;
		return true;
	end Check_Column;

	function Mark_Reduce_Total(The_Board: in out Board; Total, Draw: in Integer) return Integer
	is
		result: Integer := Total;
	begin
		for I in BoardWidth loop
			for J in BoardWidth loop
				if The_Board(I, J).Num = Draw then
					The_Board(I, J).Marked := True;
					return Total - Draw;
				end if;
			end loop;
		end loop;
		return result;
	end Mark_Reduce_Total;
	task body BoardProcessor is
		Id: Integer := 0;
		Total: Integer := 0;
		DrawCount: Integer := 0;
		Win_Mark: Boolean := false;
	begin
		loop
			select
			accept Add_Board(BoardId: in Integer) do
				Id := BoardId;
				for I in BoardWidth loop
					for J in BoardWidth loop
						Total := Total + Boards(BoardId)(I, J).Num;
					end loop;
				end loop;
			end Add_Board;
			or
			accept Check_Board(DrawNum: in Integer; Win: out Integer) do
				Win := 0;
				DrawCount := DrawCount + 1;
				Total := Mark_Reduce_Total(Boards(Id), Total, DrawNum);
				for I in BoardWidth loop
					Win_Mark := Check_Row(Boards(Id), I);
					if Win_Mark then
						--tio.Put_Line("Total unmarked: " & Total'image & " with draw num: " & DrawNum'image &
						--" for row " & I'image & " in board " & Id'image);
						Win := Total * DrawNum;
						exit;
					end if;
					Win_Mark := Check_Column(Boards(Id), I);
					if Win_Mark then
						--tio.Put_Line("Total unmarked: " & Total'image & " with draw num: " & DrawNum'image &
						--" for col " & I'image & " in board " & Id'image);
						Win := Total * DrawNum;
						exit;
					end if;
				end loop;
			end Check_Board;
			or
			accept Done do return; end Done;
			or terminate;
		end select;
		end loop;
	end BoardProcessor;

	processor: array(1..3) of BoardProcessor;

	procedure Board_Drawing is
	begin
		for Bds in 1 ..3 loop
			tio.Put_Line("=== board" & Bds'image);
			for I in BoardWidth loop
				for J in BoardWidth loop
					tio.Put(To_String(Boards(Bds)(I, J)));
				end loop;
				tio.New_Line;
			end loop;
		end loop;
	end Board_Drawing;

	Win_Out_Total, Board_Id : Integer := 0;

begin
	DrawLen := Get_Draws;
	--tio.Put_Line("The drawlen is " & DrawLen'image);
	--for i in 1 .. DrawLen loop
		--tio.Put(Draws(i)'image);
	--end loop;
	--tio.New_Line;
	Get_Boards(DrawLen);
	--Board_Drawing;
	for I in processor'range loop
		processor(i).Add_Board(i);
	end loop;
	for I in 1 .. DrawLen loop
		if Win_Out_Total > 0 then
			exit;
		end if;
		for J in processor'range loop
			processor(J).Check_Board(Draws(I), Win_Out_Total);
			--tio.Put_Line("The win out is " & Win_Out_Total'image & " for board " & J'image);
			if Win_Out_Total > 0 then
				Board_Id := J;
				exit;
			end if;
		end loop;
	end loop;
	--tio.Put_Line("====After checking board====");
	--Board_Drawing;
	tio.Put_Line("Total win is " & Win_Out_Total'image & " from board " & Board_Id'image);
end Day_04_giant_squid;
