package Wall_Types is
	-- constrained integer
	type Channel is range 2 .. 136;

	-- derived standar integer with constrained range
	type Signal is new Integer range 1..150;

	-- floating point type
	type Level is digits 7;

	-- no wall with object signal but with smaller range
	subtype Small_Signal is Signal range 2..14;

	-- enum with 4 values
	type Color is (Red, Yellow, Green, Blue);

	-- another enum type
	type Light is (Red, Yello, Green);

	-- derived from Color with smaller range
	type Traffic is new Color range Red .. Green;
end Wall_Types;
