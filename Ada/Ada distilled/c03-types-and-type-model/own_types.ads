package Own_Types is
	-- enumerated type
	type Color is (Red, Orange, Yellow, Green, Blue, Indigo, Violet);

	-- floating type
	type Fahrenheit is digits 7 range -473.0..451.0;

	-- financial data type for accounting;
	type Money is delta 0.001 digits 12;

	-- integer type
	type Quarndex is range -3_000 .. 10_000;

	-- constrained array type
	type Vector is array(1..100) of Fahrenheit;

	-- constrained array type by Color
	type Color_Mix is array(Color) of Boolean;

	-- constrained record type
	type Inventory is record
		-- initialized string type
		Description: String(1 .. 80) := (others => ' ');
		Identifier : Positive;
	end record;

	-- pointer type for Inventory
	type Inventory_Pointer is access all Inventory;

	-- unconstrained array
	type QData is array(Positive range <>) of Quarndex;

	type Account is tagged record
		-- uninitialized string
		ID : String (1..20);
		Amount : Money := 0.0;
	end record;

	-- classwide pointer for tagged type
	type Account_Ref is access all Account'Class;
end Own_Types;
