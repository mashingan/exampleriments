package root is
begin
	Bad_Bad_Bad: exception;
	No_No_No: exception;
	type Number is private;
	function "+"(N: Number) return Number;
	function "-"(N: Number) return Number;
	function Set(To: Integer) return Number;
	function Integer(From: Number) return Integer;
	private
	type Number is range -2**32 .. 2**31-1;
end root;
