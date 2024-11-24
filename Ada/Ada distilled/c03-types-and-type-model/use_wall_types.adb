with Wall_Types;
with Ada.Unchecked_Conversion;

procedure use_wall_types is
	package wt renames Wall_Types;

	Ch1, Ch2, Ch3: wt.Channel := 42;
	Sig1, Sig2: wt.Signal := 27;
	Level_1, Level_2 : wt.Level := 360.0;
	Tiny : wt.Small_Signal := 4;
	Color_1, Color_2 : wt.Color := wt.Red;
	Light_1, Light_2 : wt.Light := wt.Red;
	Traffic_1, Traffic_2 : wt.Traffic := wt.Red;
	Traffic_3 : wt.Traffic;

	use type wt.Channel;

	function Convert is new Ada.Unchecked_Conversion
	(Source => wt.Light, Target => wt.Traffic);

begin
	-- use type makes + operator visible
	Ch3 := Ch1 + Ch2;

	-- type conversion is legal between numeric types
	Level_1 := wt.Level(Ch1);

	-- ok because of subtype;
	Tiny := Sig1;

	-- ok, traffic is derived from color
	Traffic_3 := wt.Traffic(Color_1);

	-- this is ok
	Traffic_1 := wt.Traffic'Succ(Traffic_2);

	-- assign not similar data without checking
	Traffic_2 := Convert(Light_1);

	-- ilegal, convert only one way
	-- Light_2 := Convert(Traffic_1);

end use_wall_types;
