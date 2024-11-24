with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

procedure trig_num is
	package tio renames Ada.Text_IO;
	package fio renames Ada.Float_Text_IO;
	package compute is new Ada.Numerics.Generic_Elementary_Functions
	(Float_Type => Float);
	Pi : Float := Ada.Numerics.Pi;
	Radius : Float := 12.0;
	Area : Float := 0.0;
	sqrt_res : Float := 0.0;
begin
	Area := Pi * Radius ** 2;
	tio.Put("radius: ");
	fio.Put(Radius, Fore => 4, Aft => 3, Exp => 0);
	tio.New_line;
	tio.Put("area: ");
	fio.Put(Area, Fore => 4, Aft => 3, Exp => 0);
	tio.New_line;
	tio.Put("sqrt(area): ");
	sqrt_res := compute.Sqrt(Area);
	fio.Put(sqrt_res, Fore => 4, Aft => 3, Exp => 0);
	tio.New_line;
end trig_num;
