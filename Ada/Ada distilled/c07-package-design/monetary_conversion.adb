package body monetary_conversions is
	function Convert(Y: Yen, Rate: Money) return Dollars is
		Result : Dollars := 0.0;
	begin
		return Result;
	end Convert;
	function Convert(D: Dollars, Rate: Money) return Yen is
		Result : Yen := 0.0;
	begin
		return Result;
	end Convert;
end monetary_conversions;
