with Wall_Types;

procedure wont_compile is
	package wt renames Wall_Types;
	Ch1, Ch2, Ch3: wt.Channel := 42;
	Sig1, Sig2: wt.Signal := 27;
	Level_1, Level_2 : wt.Level := 360.0;
	Tiny : wt.Small_Signal := 4;
	Color_1, Color_2 : wt.Color := wt.Red;
	Light_1, Light_2 : wt.Light := wt.Red;
	Traffic_1, Traffic_2 : wt.Traffic := wt.Red;
	Light_3 : wt.Light;
	Traffic_3 : wt.Traffic;
begin
	-- cannot compile, operator + is not directly visible
	Ch3 := Ch1 + Ch2;

	-- incompatible data types
	Level_1 := Ch1;

	-- ok because of subtype;
	Tiny := Sig1;

	-- incompatible type in assignment;
	Color_1 := Light_1;

	-- incompatible type
	Light_2 := Traffic_1;

	-- type conversion is not allowed
	Light_3 := wt.Light(Color_1);

	-- incompatible type
	Traffic_3 := Color_1;

	-- this is ok
	Traffic_1 := wt.Traffic'Succ(Traffic_2);
end wont_compile;
