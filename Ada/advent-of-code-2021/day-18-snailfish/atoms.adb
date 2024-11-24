package body Atoms is
	function "="(A1, A2: in Atom) return Boolean is
	begin
		if A1.Kind  /= A2.Kind then return false; end if;
		if A1.Kind = Single then return A1.Value = A2.Value; end if;
		for I in A1.Values'range loop
			if A1.Values(I).all /= A2.Values(I).all then return false; end if;
		end loop;
		return true;
	end "=";

	function Get_Access(a: in Atom) return Atom_Access is
		(new Atom'(a.parent, a.kind, a.value, a.values));

	function "+"(A1, A2: in Atom) return Atom is
		res : Atom;
		acc1, acc2, resaccess: Atom_Access;
	begin
		res := (Parent => null, Kind => Pair, Value => 0, Values => (null, null));
		resaccess := Get_Access(res);
		acc1 := Get_Access(A1);
		acc2 := Get_Access(A2);
		acc1.parent := resaccess;
		acc2.parent := resaccess;
		if acc1.Kind = Pair then
			for I in 1..2 loop acc1.Values(I).Parent := acc1; end loop;
		end if;
		if acc2.Kind = Pair then
			for I in 1..2 loop acc2.Values(I).Parent := acc2; end loop;
		end if;
		resaccess.Values := (acc1, acc2);
		return resaccess.all;
	end "+";

	function Read_Atom(s: String; last: in out Integer;
		parent: in Atom_Access := null) return Atom
	is
		res : Atom;
		chr : Character;
		num : Integer;
	begin
		--tio.Put_Line("Read_Atom: s(last..length):  " & s(last .. s'last));
		--tio.Put_Line("last:" & last'image & ", s'last:" & s'last'image);
		if last < s'last then
			chr := s(last);
			if chr = '[' then
				last := last + 1;
				res := Read_Pair(s, last, parent);
			else
				nio.Get(s(last..s'last), num, last);
				last := last + 1;
				res := (Parent => parent, Kind => Single, Value => num, Values => (null, null));
			end if;
		end if;
		return res;
	end Read_Atom;

	function Read_Pair(s: String; last: in out Integer;
		parent: in Atom_Access := null) return Atom is
		res : Atom;
		v1, v2, respair : Atom_Access;
	begin
		respair := new Atom'(parent => parent, kind => Pair, value => 0,
		values => (null, null));
		res := Read_Atom(s, last, respair);
		--tio.Put_Line("Read_Pair: s(last..length):  " & s(last .. s'last));
		--tio.Put_Line("last:" & last'image & ", s'last:" & s'last'image);
		if last < s'last then
			if s(last) /= ',' then
				raise Invalid_Input;
			end if;
			last := last + 1;
			v1 := Get_Access(res);
			res := Read_Atom(s, last, respair);
			if s(last) /= ']' then
				raise Invalid_Input;
			end if;
			last := last + 1;
			v2 := Get_Access(res);
		end if;
		respair.Values := (v1, v2);
		return respair.all;
	end Read_Pair;

	function To_String(a: in Atom) return String is
		use bs;
		buf : Bounded_String;
		strnum : String(1..3);
	begin
		if a.Kind = Pair then
			buf := buf & '[';
			for I in 1..2 loop
				if a.Values(I) = null then
					buf := buf & "null";
				else
					buf := buf & To_String(a.values(I).all);
				end if;
				if I = 1 then buf := buf & ','; end if;
			end loop;
			buf := buf & ']';
		else
			nio.Put(strnum, a.Value);
			if a.Value > 9 and a.Value < 100 then
				buf := buf & strnum(2..3);
			else
				buf := buf & strnum(3);
			end if;
			--buf := buf & strnum;
		end if;
		return To_String(buf);
	end To_String;

	function Fetch_Most_Left_Pair(a: in Atom; depth, target: in Integer;
		theatom : out Atom_Access) return Boolean
	is
	begin
		--tio.Put_Line ("is a.parent null? " & Boolean'Image(a.parent = null));
		if a.kind = Single then return false; end if;
		--for I in 1..2 loop
			--tio.Put_Line ("I:" & I'image & ", a.Values(I).parent null? " & Boolean'Image(a.Values(I).Parent = null));
		--end loop;
		if depth < target then
			for I in 1..2 loop
				if Fetch_Most_Left_Pair(a.Values(I).all, depth+1, target, theatom) then
					return true;
				end if;
			end loop;
		else
			if a.Values(1).Kind = Single and a.Values(2).Kind = Single then
				theatom := a.Values(1).parent;
				--tio.Put_Line ("is a.Values(1).parent null? " & Boolean'Image(a.Values(1).parent = null));
				--tio.Put_Line ("is theatom.parent null? " & Boolean'Image(theatom.parent = null));
				return true;
			end if;
			for I in 1..2 loop
				if Fetch_Most_Left_Pair(a.Values(I).all, depth+1, target, theatom) then
					return true;
				end if;
			end loop;
		end if;
		return False;
	end Fetch_Most_Left_Pair;

	function Explode_Zero(a: in out Atom; target : in Atom) return Boolean
	is
	begin
		if a.kind = Single then return false; end if;
		if a = target then
			a := (Parent => a.parent, Kind => Single, Value => 0, Values => (null, null));
			return true;
		end if;
		for I in 1..2 loop
			if Explode_Zero(a.Values(I).all, target) then
				return true;
			end if;
		end loop;
		return False;
	end Explode_Zero;

	--function explode(a: in out Atom) return Boolean is
		--depthatom : Atom_Access;
		--tempatom : Atom_Access;
		--newatom : Atom;
	--begin
		----tio.Put_Line ("in explode");
		--if not Fetch_Most_Left_Pair(a, 0, 4, depthatom) then
			--return false;
		--end if;
		--tempatom := depthatom.parent;
		----tio.Put_Line("depthatom: " & To_String (depthatom.all));
		----tio.Put_Line ("is depthatom.Parent null? " & Boolean'Image(depthatom.Parent = null));
		--loop
			--exit when tempatom = null or else tempatom.kind = Single;
			----tio.Put_Line ("curr tempatom: " & To_String (tempatom.all));
			----tio.Put_Line ("curr tempatom.Kind: " & tempatom.Kind'image);
			--exit when tempatom.Values(2).all = depthatom.all and tempatom.Values(1).Kind = Single;
			--tempatom := tempatom.parent;
		--end loop;
		--if tempatom /= null then
			--if tempatom.Kind = Single then
				--tempatom.value := tempatom.value + depthatom.Values(1).value;
			--else
				--tempatom.values(1).Value := tempatom.values(1).value + depthatom.Values(1).value;
			--end if;
		--end if;
		--depthatom.Values(1).Value := 0;
		--tempatom := depthatom.parent;
		--if tempatom.Values(1).all = depthatom.all then
			--tempatom := tempatom.Values(2);
		--elsif tempatom.Values(2).all = depthatom.all then
			--tempatom := tempatom.parent.Values(2);
		--end if;
		--loop
			--exit when tempatom = null or else tempatom.kind = Single;
			--tempatom := tempatom.Values(1);
		--end loop;
		--tio.Put_Line ("tempatom 2nd: " & To_String (tempatom.all));
		--if tempatom /= null then
			--if tempatom.Kind = Single then
				--tempatom.Value := tempatom.Value + depthatom.Values(2).Value;
			--else
				--tempatom.Values(1).Value := tempatom.Values(1).Value + depthatom.Values(2).Value;
			--end if;
		--end if;
		--depthatom.Values(2).Value := 0;
		--loop exit when not Explode_Zero(a, depthatom.all); end loop;
		--return true;
	--end explode;
	function explode(a: in out Atom) return Boolean is
		depthatom : Atom_Access;
		tempatom : Atom_Access;
		firstpos : Boolean := false;
	begin
		--tio.Put_Line ("in explode");
		if not Fetch_Most_Left_Pair(a, 0, 4, depthatom) then
			return false;
		end if;
		tempatom := depthatom.parent;
		if tempatom = null then return false; end if;
		if tempatom.Values(1).all = depthatom.all then
			--tempatom := tempatom.Parent;
			firstpos := true;
			tio.Put_Line ("tempatom firstpos: " & To_String (tempatom.all));
		else
			tio.Put_Line ("tempatom secondpos: " & To_String (tempatom.all));
		end if;
		if tempatom.Kind = Single then
			tempatom.value := tempatom.value + depthatom.Values(1).Value;
		else
			tempatom.Values(1).value := tempatom.Values(1).value + depthatom.Values(1).Value;
		end if;
		if firstpos then
			tempatom := depthatom.Parent.Values(2);
			tio.Put_Line ("tempatom firstpos: " & To_String (tempatom.all));
		else
			tempatom := depthatom.Parent.Parent.Values(1);
			tio.Put_Line ("tempatom secondpos: " & To_String (tempatom.all));
		end if;
		if tempatom.Kind = Single then
			tempatom.Value := tempatom.Value + depthatom.Values(2).Value;
		else
			tempatom.Values(1).Value := tempatom.Values(1).Value + depthatom.Values(2).Value;
		end if;
		depthatom.Values(1).Value := 0;
		depthatom.Values(2).Value := 0;
		loop exit when not Explode_Zero(a, depthatom.all); end loop;
		return true;
	end explode;
end Atoms;
