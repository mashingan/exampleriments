package composites is
	-- one dimensional unconstrained array
	type Array_Type is array(Index_Type range <>) of Component_Type;

	-- one dimensional constrained array
	type Array_Type_2 is array(Range_Constraint) of Component_Type;

	type Float_Matrix is array(Integer range <>, Positive range <>) of Float;
	type Bool_matrix is array(Natural range <>,
		Positive range <>,
		Color range <>
		) of Boolean;
	type Mat_Vector is array (Positive range <>) of Float_Matrix(1..20, 5..15);
end composites;
