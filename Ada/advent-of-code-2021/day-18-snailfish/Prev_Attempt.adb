package body Prev_Attempt is

	procedure Read_Lines is
		line : string (1..128);
		length : Integer := 0;
		atom : Atoms.Atom;
		last : Positive := 1;
	begin
		loop
			begin
				tio.Get_Line(line, length);
			exception
				when tio.End_Error => exit;
				when tio.Data_Error => exit;
			end;
			exit when length = 0;
			atom := Atoms.Read_Atom(line(1..length), last);
			lines.Append(atom);
			--tio.Put_Line("Read_Lines: " & line(1..length));
			last := 1;
		end loop;
	end Read_Lines;

	procedure reduce(vec: in out npr.Num_Vector.Vector; check: Boolean := false) is
		pss : npr.Pos_Vector.Vector := npr.Pos_Vector.Empty_Vector;
		explode_count, split_count, readjust_count : Integer := 0;
	begin
		loop
			if npr.explode(vec) then
				explode_count := explode_count + 1;
			end if;
			if check then
				tio.Put_Line("after exploding");
				npr.check_vecs(vec);
			end if;
			if npr.split(vec) then
				split_count := split_count + 1;
			end if;
			if check then
				tio.Put_Line("after split");
				npr.check_vecs(vec);
			end if;
			exit when split_count = 0 and explode_count = 0;
			split_count := 0;
			explode_count := 0;
		end loop;
	end reduce;

	procedure reduce(atom: in Atoms.Atom; check: Boolean := false) is
		pss : npr.Pos_Vector.Vector := npr.Pos_Vector.Empty_Vector;
		vec : npr.Num_Vector.Vector := npr.Num_Vector.Empty_Vector;
		explode_count, split_count : Integer := 0;
	begin
		npr.To_Num_Pos(atom, npr.First, pss, vec);
		reduce(vec, check);
	end reduce;


	procedure reduce(s: in String; check: Boolean := false) is
		last : Positive := 1;
		atom : Atoms.Atom := Atoms.Read_Atom(s, last);
	begin
		reduce(atom, check);
	end reduce;

	procedure sep(s: String := "new example") is
	begin
		tio.New_Line;
		tio.Put_Line("===" & s & "===");
		tio.New_Line;
	end sep;

	procedure prev_attempt is
		last : Integer := 1;
		--atom : Atoms.Atom;
		--a1, a2: Atoms.Atom_Access;
		vec : npr.Num_Vector.Vector;
		pss : npr.Pos_Vector.Vector;
		expl_sample_1 : String := "[[[[4,3],4],4],[7,[[8,4],9]]]";
		expl_sample_2 : String := "[1,1]";
		package nv renames npr.Num_Vector;
		use npr;
		use Pos_Vector;
		use Num_Vector;
	begin
		--atom := Atoms.Read_Atom(expl_sample_1, last);
		--a1 := Atoms.Get_Access(atom);
		--last := 1;
		--atom := Atoms.Read_Atom(expl_sample_2, last);
		--a2 := Atoms.Get_Access(atom);
		--atom := (Atoms.Pair, (a1, a2));
		--tio.Put_Line("addition: " & Atoms.To_String(atom));
		--reduce(atom, true);
		--sep;
		--reduce("[[[[[9,8],1],2],3],4]", true);
		--sep;
		--reduce("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", true);

		--last := 1;
		--pss.Clear;
		--vec.Clear;
		--npr.To_Num_Pos(Atoms.Read_Atom(expl_sample_1, last), First, pss, vec);
		--last := 1;
		--vec := vec + Atoms.Read_Atom(expl_sample_2, last);
		--sep("+");
		--check_vecs(vec);
		--reduce(vec);
		--sep("reduced+");
		--check_vecs(vec);

		npr.To_Num_Pos(lines.First_Element, npr.First, pss, vec);
		sep("init vec");
		check_vecs(vec);
		for I in lines.First_Index+1 .. lines.Last_Index loop
			tio.Put_Line("next: " & Atoms.To_String(lines(I)));
			vec := vec + lines(I);
			sep("After addition");
			check_vecs(vec);
			--reduce(vec, true);
			reduce(vec);
			sep("after reduce");
			check_vecs(vec);
		end loop;
		sep("final reduced");
		check_vecs(vec);
	end prev_attempt;
end Prev_Attempt;
