with Ada.Containers;

package body Num_Pos_Repr is
	use Num_Vector;
	use Pos_Vector;

	package Pos_String is new Ada.Strings.Bounded.Generic_Bounded_Length
		(Max => 128);

	function To_String(p: in pv.Vector) return String is
		use Pos_String;

		buf: Bounded_String;
	begin
		buf := buf & '(';
		for C in p.Iterate loop
			if p(C) = First then
				buf := buf & '1';
			else
				buf := buf & '2';
			end if;
		end loop;
		return To_String(buf & ')');
	end To_String;

	function "="(n1, n2 : in Num_Pos) return Boolean is
		(n1.Num = n2.Num and n1.pos = n2.pos);

	procedure To_Num_Pos(a: in Atoms.Atom; thepos: in Pos;
		posvector: in out Pos_Vector.vector; vec: in out nv.Vector)
	is
		use Atoms;
		anotherpos : Pos;
	begin
		if a.Kind = Single then
			vec.Append((a.Value, posvector));
		else
			for I in 1..2 loop
				if a.Values(I) /= null then
					if I = 1 then
						anotherpos := First;
					else
						anotherpos := Second;
					end if;
					posvector.Append(anotherpos);
					To_Num_Pos(a.Values(I).all, anotherpos, posvector, vec);
					posvector.Delete_Last;
				end if;
			end loop;
		end if;
	end To_Num_Pos;

	function "+"(n1, n2: in Num_Vector.Vector) return Num_Vector.Vector is
		use Num_Vector;
		use Ada.Containers;
		newres : nv.Vector := nv.Empty_Vector;
		np : Num_Pos;
	begin
		newres.Reserve_Capacity(Count_Type(n1.Length) + Count_Type(n2.Length));
		for N in n1.Iterate loop
			np := n1(N);
			np.pos.Prepend(First);
			newres.Append(np);
		end loop;
		for N in n2.Iterate loop
			np := n2(N);
			np.pos.Prepend(Second);
			newres.Append(np);
		end loop;
		return newres;
	end "+";

	function "+"(n1: in Num_Vector.Vector; atom: in Atoms.Atom) return Num_Vector.Vector is
		vec : nv.Vector := nv.Empty_Vector;
		pss : pv.Vector := pv.Empty_Vector;
	begin
		To_Num_Pos(atom, first, pss, vec);
		return n1 + vec;
	end "+";

	function "+"(atom: in Atoms.Atom; n1: in Num_Vector.Vector) return Num_Vector.Vector is
		(n1 + atom);

	function readjust(vec: in out Num_Vector.Vector) return Boolean is
		use Ada.Containers;
		firstidx : Integer := vec.First_Index;
		lastidx  : Integer := vec.Last_Index;
		npo, nptemp : Num_Pos;
		bufpos : Pos_Vector.Vector := Pos_Vector.Empty_Vector;
	begin
		for I in firstidx .. lastidx loop
			npo := vec(I);
			if npo.pos.Last_Element = Second and I-1 >= firstidx then
				nptemp := vec(I-1);
				if npo.pos.Length > nptemp.pos.Length or 
					(npo.pos.Length = nptemp.pos.Length and nptemp.pos.Last_Element /= First) then
					bufpos := npo.pos;
					bufpos(bufpos.Last_Index) := First;
					vec.Insert(I, (0, bufpos));
					return true;
				end if;
			elsif npo.pos.Last_Element = First and I+1 <= lastidx then
				nptemp := vec(I+1);
				if npo.pos.Length > nptemp.pos.Length or
					(npo.pos.Length = nptemp.pos.Length and nptemp.pos.Last_Element /= Second) then
					bufpos := npo.pos;
					bufpos(bufpos.Last_Index) := Second;
					vec.Insert(I+1, (0, bufpos));
					return true;
				end if;
			end if;
		end loop;
		return false;
	end readjust;

	function explode(vec: in out Num_Vector.Vector) return Boolean is
		use Ada.Containers;
		npo, nptemp, npnext : Num_Pos;
		firstidx : Natural := vec.First_Index;
		lastidx : Natural := vec.Last_Index;
		prevdel, nextdel: Integer := -1;
		J : Integer;
		bufpos : Pos_Vector.Vector := Pos_Vector.Empty_Vector;
	begin
		for I in firstidx .. lastidx loop
			J := I;
			npo := vec(I);
			if Integer(npo.pos.Length) < 5 then
				goto Continue_Loop;
			end if;

			if npo.pos.Last_Element = First and (J + 1) <= lastidx then
				--tJo.Put_LJne("got 4th deep fJrst");
				npnext := vec(J+1);
				if npnext.pos.Last_Element /= Second or npnext.pos.Length /= npo.pos.Length then
					goto Continue_Loop;
				end if;
				--tJo.Put_LJne("got 4th deep second");
				if J-1 >= firstidx then
					--tio.Put_LJne("Got prev node");
					nptemp := vec(J-1);
					nptemp.Num := npo.Num + nptemp.Num;
					npo.Num := 0;
					vec(J) := npo;
					vec(J-1) := nptemp;
					prevdel := J;
				else
					--tio.Put_LJne("No prev node:" & J'Jmage);
					bufpos := npo.pos.Copy(npo.pos.Length);
					bufpos.Delete_Last;
					nptemp := (0, bufpos);
					npo.Num := 0;
					vec(J) := npo;
					vec.Prepend(nptemp);
					J := J+1;
					prevdel := J;
				end if;
				--tJo.Put_LJne("check for second paJr");
				if J+2 > lastidx then
					--tJo.Put_LJne("no next of next node");
					bufpos := npnext.pos.Copy(npnext.pos.Length);
					bufpos.Delete_Last;
					nptemp := (0, bufpos);
					npnext.Num := 0;
					vec.Append(nptemp);
					vec(J+1) := npnext;
					nextdel := J+1;
				else
					--tio.Put_Line("get next of next node");
					nptemp := vec(J+2);
					nptemp.Num := npnext.Num + nptemp.Num;
					npnext.Num := 0;
					vec(J+1) := npnext;
					vec(J+2) := nptemp;
					nextdel := J+1;
				end if;
				--tio.Put_Line("Current before del");
				--check_vecs(vec);
				if nextdel > -1 then
					vec.Delete(nextdel, Count_Type(1));
				end if;
				if prevdel > -1 then
					vec.Delete(prevdel, Count_Type(1));
				end if;
				loop exit when not readjust(vec); end loop;
				--if readjust(vec) then null; end if;
				return true;
			end if;
			<<Continue_Loop>>
		end loop;
		return false;
	end explode;

	procedure explode(vec: in out Num_Vector.Vector) is
		dummy: Boolean;
	begin
		dummy := explode(vec);
	end explode;

	function split(vec: in out Num_Vector.Vector) return Boolean is
		use Num_Vector;
		firstidx : Natural := vec.First_Index;
		lastidx : Natural := vec.Last_Index;
		J : Natural := 0;
		np : Num_Pos;
		headnum, tailnum : Natural := 0;
	begin
		for I in firstidx .. lastidx loop
			J := I;
			np := vec(J);
			if np.Num > 9 then
				headnum := np.Num / 2;
				tailnum := np.Num - headnum;
				vec.Insert(J, (headnum, np.pos & To_Vector(First, 1)));
				vec.Insert(J+1, (tailnum, np.pos & To_Vector(Second, 1)));
				vec.Delete(J+2);
				return true;
			end if;
		end loop;
		loop exit when not readjust(vec); end loop;
		return false;
	end split;

	procedure check_vecs(vec: in Num_Vector.Vector) is
	begin
		for C in vec.Iterate loop
			tio.Put_Line("pos:" & To_String(vec(C).pos) & " => Num:" & vec(C).Num'image);
		end loop;
	end check_vecs;

end Num_Pos_Repr;
