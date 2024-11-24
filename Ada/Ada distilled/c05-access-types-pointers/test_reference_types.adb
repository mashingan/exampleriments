with reference_types;
with reference_functions;
with Ada.Text_IO;

procedure test_reference_types is
	Test_data : reference_types.Int_32 := 42;
	package Int_32_IO is new Ada.Text_IO.Integer_IO
	(Num => reference_types.Int_32);

	Test_data_set : reference_types.Data_set(0..20) :=
		(others => Test_data);
begin
	reference_types.Process(
		Data => Test_data_set,
		Method => reference_functions.My_Process);

end test_reference_types;
