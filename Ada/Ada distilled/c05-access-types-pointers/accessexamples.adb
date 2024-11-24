with Ada.Integer_Text_IO;

procedure accessexamples is
	package nio renames Ada.Integer_Text_IO;
	type Integer_Pointer is access Integer; -- storage pool access
	
	procedure Access_Type_1 is
		Number: Integer := 42;
		Location : Integer_Pointer;
	begin
		Location := new Integer;
		Location.all := Number; -- dereference
		--nio.Put(Location); -- illegal
		nio.Put(Location.all);
	end Access_Type_1;

	type Integer_Pointer_All is access all Integer; -- storage pool access

	procedure Access_Type_2 is
		N1: aliased Integer := 42;
		N2: Integer := 360;
		Location : Integer_Pointer_All;
	begin
		Location := N1'access; -- point to N1
		--nio.Put(Location); -- illegal
		nio.Put(Location.all);
		--Location := N2'access; -- illegal
	end Access_Type_2;

	procedure Access_Type_3 is
		Location : Integer_Pointer_All;
	begin
		declare
			N1 : aliased Integer := 42;
		begin
			-- illegal because of different scope level
			-- N1 is scoped within declare local body
			-- while Location scoped within procedure declaration
			--Location := N1'access;
		end;
	end Access_Type_3;

	procedure Accessibility_Problem_1 is
		type Integer_Reference is access all Integer;
		Reference : Integer_Reference;
		Data : aliased Integer;
	begin
		Reference := Data'access;
	end Accessibility_Problem_1;

	procedure Accessibility_Problem_2 is
		type Integer_Reference is access all Integer;
		Reference : Integer_Reference;
	begin
		declare
			Data : aliased Integer;
		begin
			-- illegal, because different scope
			Reference := Data'access;
		end;
	end Accessibility_Problem_2;

	procedure Accessibility_Problem_3 is
		type Integer_Reference is access all Integer;
	begin
		declare
			Reference : Integer_Reference;
			Data : aliased Integer;
		begin
			-- illegal, because reference must exists as long procedure run
			Reference := Data'access;
		end;
	end Accessibility_Problem_3;

begin
	Access_Type_1;
	Access_Type_2;
	Access_Type_3;
end accessexamples;
