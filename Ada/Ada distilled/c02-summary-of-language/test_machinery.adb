with Machinery_1_3;
with Ada.Text_IO;

procedure test_machinery is
	package mach renames Machinery_1_3;
	package tio renames Ada.Text_IO;
	Widget: mach.Machine;
	procedure report_machine(m: in mach.Machine) is
		is_on: boolean;
	begin
		is_on := mach.Is_on(m);
		tio.Put_Line("is machinery on? " & is_on'image);
	end report_machine;
begin
	report_machine(widget);
	mach.Turn_On(M => Widget);
	report_machine(widget);
	tio.Put_Line("turn off machinery");
	mach.Turn_Off(M => Widget);
	report_machine(widget);
	tio.Put_Line("machine safely worked on/off");
end test_machinery;
