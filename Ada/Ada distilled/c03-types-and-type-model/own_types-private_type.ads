package Own_Types.Private_Type is
	type Inventory is limited private;
	type Inventory_Pointer is access all Inventory;
	procedure Create(Inv: in out Inventory);
	type Account is tagged private;
	type Account_Ref is access all Account'class;
	function Create(Id: String; Amt: Float) return Account_Ref;

	private
	Current_Identifier : Positive := 1;
	type Inventory is record
		Description: String(1 .. 80) := (others => ' ');
		Identifier : Positive;
	end record;

	type Account is tagged record
		ID : String(1 .. 12);
		Amount : Float := 0.0;
	end record;
end Own_Types.Private_Type;
