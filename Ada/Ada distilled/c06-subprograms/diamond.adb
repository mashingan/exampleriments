with Ada.Text_IO;

procedure Diamond is
	package tio renames Ada.Text_IO;
	subtype Column is tio.Positive_Count;
	Center : constant := 37;
	Left_Temp, Right_Temp : Integer := Center;
	Plus_2 : constant := 2;
	Minus_2 : constant := -2;
	procedure Draw (Left, Right, Depth: in Integer) is
		Symbol : String(1..1) := "X";
		Left_col, Right_col: Column;
	begin
		for Index in 1..Depth loop
			if Left_Temp = Center then
				tio.Set_col(Center);
				tio.Put(Symbol);
			else
				Left_col := Column(Left_Temp);
				Right_col := Column(Right_Temp);
				tio.Set_col(Left_col);
				tio.Put(Symbol);
				tio.Set_col(Right_col);
				tio.Put(Symbol);
			end if;
			tio.New_line;
			Left_Temp := Left_Temp + Left;
			Right_Temp := Right_Temp + right;
		end loop;
	end Draw;
begin
	Draw(Left => Minus_2, Right => Plus_2, Depth => 9);
	Draw(Left => Plus_2, Right => Minus_2, Depth => 10);
end Diamond;
