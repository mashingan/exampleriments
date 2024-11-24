package body reference_functions is
	procedure My_Process(Data: reference_types.Int_32) is
	begin
		null;
	end My_Process;

	function My_Validator(Data: reference_types.Int_32) return Boolean is
	begin
		return True;
	end My_Validator;

	function My_Process return reference_types.Process_Method is
		Test_Process : reference_types.Process_Method := My_Process'access;
	begin
		return Test_Process;
	end My_Process;
	function My_Validator return reference_types.Validate_Routine is
		Test_validator : reference_types.Validate_Routine := My_Validator'access;
	begin
		return Test_validator;
	end My_Validator;
end reference_functions;
