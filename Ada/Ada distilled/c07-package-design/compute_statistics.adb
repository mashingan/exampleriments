with statistics;
with Ada.Float_Text_IO;
with Ada.Text_IO;
with Ada.Numerics.Float_Random;

procedure compute_statistics is
	package frand renames Ada.Numerics.Float_Random;
	package fio renames Ada.Float_Text_IO;
	package tio renames Ada.Text_IO;
	Stat_Data : statistics.Data(1..100);
	gen : frand.Generator;
begin
	frand.Reset(gen);
	for I in Stat_Data'range loop
		Stat_Data(I) := frand.Random(gen);
		fio.Put(Stat_Data(I), Fore => 4, Aft => 3, Exp => 0);
		tio.Put(" ");
	end loop;
	tio.New_line;
	tio.Put("mean: ");
	fio.Put(statistics.Mean(Stat_Data), Fore => 1, Aft => 5, Exp => 0);
	tio.New_line;
	tio.Put("variance: ");
	fio.Put(statistics.Variance(Stat_Data), Fore => 3, Aft => 3, Exp => 0);
	tio.New_line;
	tio.Put("standard dev: ");
	fio.Put(statistics.StdDev(Stat_Data), Fore => 3, Aft => 3, Exp => 0);
end compute_statistics;
