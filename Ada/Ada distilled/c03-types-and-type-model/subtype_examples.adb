procedure Subtype_Examples is
	type Frequency is digits 12;
	subtype Full_Frequency is Frequency range 0.0 .. 100_000.0;
	subtype High_Frequency is Frequency range 20_000.0 .. 100_000.0;
	subtype Low_Frequency is Frequency range 0.0 .. 20_000.0;

	FF: Full_Frequency := 0.0;
	HF: High_Frequency := 50_000.0;
	LF: Low_Frequency  := 15_000.0;
begin
	FF := HF; -- ok, no possible constrained error
	FF := LF; -- ok, no possible constrained error
	LF := FF; -- ok, but potential constrained error
	HF := LF; -- ok, but potential constrained error
end Subtype_Examples;
