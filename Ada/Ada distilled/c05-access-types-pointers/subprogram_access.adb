with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Float_Text_IO;
procedure subprogram_access is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	type Action is access procedure(Data: in out Integer);
	procedure Process(D: in out Integer) is
	begin
		D := D + D;
	end Process;

	type Process_Set is array(1..10) of Action;
	Index : Positive;
	Value : Integer := 0;
	The_process : Process_set := (others => Process'access);
	Last_get : Positive;

	procedure Access_procedure is
	begin
		loop
			Try_Get_Data:
			begin
				tio.Put("Enter index(1..10): ");
				nio.Get(tio.Get_Line, Index, Last_get);
				tio.Put("Enter Integer value: ");
				nio.Get(tio.Get_Line, Value, Last_get);
			exception
				when E : tio.Data_Error =>
					tio.Put_LIne("invalid data inputted, use default 1");
					tio.Put_LIne("invalid integer value, use default 1");
					Index := 1;
					Value := 1;
				when E : Constraint_Error =>
					tio.Put_Line("index not positive, exit loop");
			end Try_Get_Data;
			exit when Index not in 1..10;
			The_process(Index)(Data => Value);
			tio.Put_Line("The result for index " & Positive'image(Index) &
				" is " & Integer'image(Value));
		end loop;
	end Access_procedure;

	type Real is digits 12;
	function Function_Access_Type(D: in Real) return Real is
	begin
		return D + D;
	end Function_Access_Type;

	type Func_Compute is access function(D: in Real) return Real;
	procedure Func_Process(Behaviour: Func_Compute; Input: in Real;
		Output: out Real) is
	begin
		Output := Behaviour(Input);
	end Func_Process;

	procedure Access_Function is
		package fio is new tio.Float_IO(Num => Real);
		Result, Value : Real := 0.0;
	begin
		loop
			Try_Get_Data:
			begin
				tio.Put("Enter real value (0 to exit): ");
				fio.Get(tio.Get_Line, Value, Last_get);
				exit when Value = 0.0;
			exception
				when E : tio.Data_Error =>
					tio.Put_LIne("invalid value inputted, use default 1.0");
					Value := 1.0;
				when E : Constraint_Error =>
					tio.Put_Line("index not positive, exit");
					exit;
			end Try_Get_Data;
			Func_Process(Behaviour => Function_Access_Type'access,
				Input => Value, Output => Result);
			tio.Put("The result is ");
			fio.Put(Result, Fore => 4, Aft => 3, Exp => 0);
			tio.New_Line;
		end loop;
	end Access_Function;
begin
	Access_procedure;
	Access_Function;
end subprogram_access;
