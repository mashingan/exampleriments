package monetary_conversions is
	type Money is delta 0.0001 digits;
	type Yen is new Money;
	type Dollars is new Money;
	function Convert(Y: Yen, Rate: Money) return Dollars;
	function Convert(D: Dollars, Rate: Money) return Yen;
	Conversion_Error: exception;
end monetary_conversions;
