package reference_types is
	type Int_32 is range -2**31..2**31-1;
	for Int_32'size use 32;
	type Data_set is array(Natural range <>) of Int_32;
	type Data_set_reference is access all Data_set;
	type Validate_Routine is access function(Data: Int_32) return Boolean;
	type Process_Method is access procedure(Data: Int_32);
	procedure Process (Data: in out Data_set; Method: in Process_Method);
	function Validate(Data: access Data_set; Validator: in Validate_Routine) return Boolean;
	function Validate(Data: in Data_set; Validator: in Validate_Routine) return Boolean;
end reference_types;
