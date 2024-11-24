package body Own_Types.Private_Type is
	procedure Create(inv: in out Inventory) is
	begin
		--inv.Identifier := Current_Identifier;
		inv := Inventory'(
			Identifier => Current_Identifier,
			Description => (others => ' '));
		Current_Identifier := Current_Identifier + 1;
	end Create;

	function Create(Id: String; Amt: Float) return Account_Ref
	is 
		IdStr : string(1..12);
	begin
		for index in Id'range loop
			exit when index > 12;
			IdStr(index) := Id(index);
		end loop;
		return new Account'(ID => idStr, Amount => Amt);
	end Create;
end Own_Types.Private_Type;
