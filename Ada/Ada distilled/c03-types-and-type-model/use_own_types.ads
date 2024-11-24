with Own_Types.Private_Type;

package use_own_types is
	package ot renames Own_Types.Private_Type;

	type Repair_Parts_Inventory is new ot.Inventory;

	type Liability is new ot.Account
		with record
		Credit_Value : Float;
		Debit_Value : Float;
	end record;
end use_own_types;
