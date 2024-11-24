with Ada.Text_IO;
with Ada.Containers.Vectors;
with Atoms;
with Num_Pos_Repr;

package Prev_Attempt is
	package Atom_Vector is new Ada.Containers.Vectors
		(Element_Type => Atoms.Atom, Index_Type => Natural, "=" => Atoms."=");
	package av renames Atom_Vector;

	lines : av.Vector;
	procedure Read_Lines;

	procedure reduce(vec: in out Num_Pos_Repr.Num_Vector.Vector;
		check: Boolean := false);
	procedure reduce(atom: in Atoms.Atom; check: Boolean := false);
	procedure reduce(s: in String; check: Boolean := false);

	procedure sep(s: String := "new example");

	procedure prev_attempt;

	private
	package tio renames Ada.Text_IO;
	package npr renames Num_Pos_Repr;
end Prev_Attempt;
