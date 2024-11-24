with Ada.Text_IO;

procedure Arraydef is
	-- rename packages
	package tio renames Ada.Text_IO;
	-- IO package for boolean type
	package BIO is new tio.Enumeration_IO(Enum => Boolean);
	-- constrained boolean array
	type Boolean_Set is array(1..4) of Boolean;
	pragma Pack(Boolean_Set); -- force array to four bits
	for Boolean_Set'Alignment use 2; -- align storage for 2 bytes;
	type Float_Vector is array(Natural range <>) of Float; -- unconstrained array
	type Float_matrix is array(Natural range <>, Positive range <>) of Float;

	B1 : Boolean_Set := (True, True, True, False);
	B2 : Boolean_Set := (False, False, True, False);
	B3 : Boolean_Set := (True, True, False, True);
	F1 : Float_Vector(0..9);
	F2 : Float_Vector(1..10) := (1 => 12.3, 3 => 62.2, 2 => 9.4, others => 4.2);
	M1 : Float_Matrix(1..10, 1..10) := (
		1 => (1 => 0.0, others => 0.5),
		10 => (10 => 0.0, others => 1.0),
		others => (others => 1.0));

	procedure Display(Data: Boolean_Set; Comment: String) is
	begin
		tio.Put(comment);
		for I in Data'range loop
			BIO.Put(Data(I));
			tio.Put(" ");
		end loop;
		tio.New_Line;
	end Display;
begin
	F1(2) := F2(4);
	F1(5..7) := F2(6..8);
	Display(B1, "B1 is ");
	Display(B2, "B2 is ");
	Display(B3, "B3 is ");
	B3 := B1 and B2;
	Display(B3, "B1 and B2 = ");
	B3 := B1 or B2;
	Display(B3, "B1 or B2 = ");
	B3 := B1 xor B2;
	Display(B3, "B1 xor B2 = ");
	for I in M1'range(1) loop
		for J in M1'range(2) loop
			tio.Put(Float'image(M1(I, J)) & " ");
		end loop;
		tio.New_Line;
	end loop;
end Arraydef;
