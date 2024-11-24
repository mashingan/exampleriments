package body reference_types is
	procedure Process (Data: in out Data_set; Method: in Process_Method) is
	begin
		for I in Data'range loop
			Method(Data(I));
		end loop;
	end Process;
	function Validate(Data: access Data_set; Validator: in Validate_Routine) return Boolean is
	begin
		return Validate(Data.all, Validator);
	end Validate;
	function Validate(Data: in Data_set; Validator: in Validate_Routine) return Boolean is
		Without_error : Boolean := True;
	begin
		for I in Data'range loop
			Without_error := Validator(Data => Data(I));
			exit when not Without_error;
		end loop;
		return Without_error;
	end Validate;
end reference_types;
