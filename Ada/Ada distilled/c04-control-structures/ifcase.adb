with Ada.Text_IO;
with Ada.Exceptions;
procedure ifcase is
	package tio renames Ada.Text_IO;
	function Select_Option(A, B, C : Float) return Float is
		Result : Float := 0.0;
	begin
		if A > B then
			Result := A ** 2;
		elsif A < B then
			Result := B ** 2;
		elsif A <= C then
			Result := C * B;
		else
			Result := C * A;
		end if;
		return Result;
	end Select_Option;

	type Item is range 1..20;
	function Continue(Data: Item) return Boolean is
		Result : Boolean := False;
	begin
		--if Data in 1..20 then
		if Data'valid then
			Result := True;
		end if;
		return Result;
	end Continue;

	item_data : Item := 11;

	type Bounded_Integer is new Integer range -473..451;
	
	procedure Demand(Data: in out Bounded_Integer'base) is
		Local: Bounded_Integer'Base := 0;
	begin
		Data := Data + local;
		if Data in Bounded_Integer then
			tio.Put_Line("data is in bounded");
		end if;
	end Demand;
	Data_Demand : Bounded_Integer'base;

	type Color is (White, Red, Orange, Yellow, Chartreuse, Green,
		Blue, Indigo, Violet, Black, Brown);

	function Evaluate(C: Color) return Integer is
		Result: Integer := 0;
	begin
		case C is
			when Red => Result := 1;
			when Blue => Result := 2;
			when Black..Brown => Result := 3;
			when Orange | Indigo => Result := 4;
			when others => Result := 5;
		end case;
		return Result;
	end Evaluate;
	Color_in : Color := Red;

	procedure Block_info(C: Color) is
	begin
		tio.Put_Line("In " & C'image & " block");
	end Block_info;

	function Decide(C: Color) return Integer is
		Result : Integer := 0;
	begin
		Block_info(C);
		case C is
			when Red =>
				begin
					Result := 1;
				end;
			when Blue =>
				Label_1:
				begin
					Result := 2;
				end Label_1;
			when others =>
				begin
					tio.Put_Line("In others block");
					raise Constraint_Error with "only accept Red and Blue";
				exception
					when E : Constraint_Error =>
						tio.Put_Line(Ada.Exceptions.Exception_Message(E));
				end;
		end case;
		return Result;
	end Decide;
begin
	tio.Put_Line(Select_Option(10.0, 20.0, 30.0)'image);
	tio.Put_Line(Continue(Item_Data)'image);
	Data_Demand := -500;
	Demand(Data_Demand);
	tio.Put_Line(Color_in'image & " is evaluated to " &
		Evaluate(Color_in)'image);
	Color_in := Brown;
	tio.Put_Line(Color_in'image & " is decided to " &
		Decide(Color_in)'image);
end ifcase;
