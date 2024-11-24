package application is
	type Assertion is access function return Boolean;
	Precondition_error : exception;
	Postcondition_error : exception;
	Invariant_error: exception;
end application;
