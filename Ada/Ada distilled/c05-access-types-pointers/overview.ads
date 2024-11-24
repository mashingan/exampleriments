package overviews is
	-- storage pool access type
	type Reference is access Integer;

	-- general access type
	type Float_reference is access all Float;

	-- ordinary limited type
	type Container is limited private;

	-- access to limited type
	type Container_Pointer is access all Container;

	type Method is access procedure procName;
	type Method is access function funName;
end overviews;
