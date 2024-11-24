-- compile it: (small)
-- gnatmake -O2 day_18_snailfish.adb -bargs -largs -s
-- (normal)
-- gnatmake day_18_snailfish.adb
-- run in cmd (not powershell)
-- type input.txt | day_18_snailfish.exe
-- or simply run:
-- build-run.bat
-- to clean folder: gnatclean
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;
with Ada.Containers;
--with Atoms;
--with Num_Pos_Repr;
with Prev_Attempt;
with Snailfish_Trees;
with Atoms;

procedure day_18_snailfish is
	package tio renames Ada.Text_IO;
	package pra renames Prev_Attempt;
	package sft renames Snailfish_Trees;

	procedure check_explode(s: string) is
		atom : Atoms.Atom;
		last : Positive := 1;
	begin
		atom := Atoms.Read_Atom(s, last);
		pra.sep("before explode");
		tio.Put_Line(Atoms.To_String(atom));
		--Atoms.explode(atom, dum);
		loop
			exit when not Atoms.explode(atom);
			pra.sep("atom after step explode");
			tio.Put_Line(Atoms.To_String(atom));
		end loop;
		pra.sep("atom after explode done");
		--tio.Put_Line(Atoms.To_String(atom));
	end check_explode;

	procedure check_all is
		use Atoms;
		theatom : Atoms.Atom;
	begin
		theatom := pra.Lines.First_Element;
		for I in pra.Lines.First_Index+1 .. pra.Lines.Last_Index loop
			tio.Put_Line("I:" & I'image & ", pra.Lines(I): " & Atoms.To_String(pra.Lines(I)));
			theatom := theatom + pra.Lines(I);
			pra.sep("before explode");
			tio.Put_Line(To_String(theatom));
			loop
				exit when not explode(theatom);
				pra.sep("atom after step explode");
				tio.Put_Line(To_String(theatom));
			end loop;
		end loop;
		pra.sep("atom after explode done");
	end check_all;

	atom : Atoms.Atom;
	last : Positive := 1;
begin
	pra.Read_Lines;
	--pra.prev_attempt;
	--sft.Read_Snailfishes;
	--tio.Put_Line(sft.To_String(sft.Snailfishes.First_Element));

	--atom := pra.Lines.First_Element;
	--pra.sep("first elem");
	--tio.Put_Line(Atoms.To_String(atom));
	--atom := Atoms."+"(atom, pra.Lines(pra.Lines.First_Index+1));
	--pra.sep("added with");
	--tio.Put_Line(Atoms.To_String(pra.Lines(pra.Lines.First_Index+1)));
	--pra.sep("before explode");
	--tio.Put_Line(Atoms.To_String(atom));
	--loop
		--exit when not Atoms.explode(atom);
		--pra.sep("atom after explode");
		--tio.Put_Line(Atoms.To_String(atom));
	--end loop;
	--pra.sep("atom after explode done");

	check_all;
	check_explode("[[[[[9,8],1],2],3],4]");
	check_explode ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]");
	check_explode ("[[[[[1,1],[2,2]],[3,3]],[4,4]],[5,5]]");
end day_18_snailfish;
