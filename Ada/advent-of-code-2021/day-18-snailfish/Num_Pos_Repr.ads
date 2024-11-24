with Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Containers.Vectors;
with Atoms;

package Num_Pos_Repr is
	type Pos is (First, Second);

	package Pos_Vector is new Ada.Containers.Vectors
		(Element_Type => Pos, Index_Type => Natural);

	type Num_Pos is record
		Num : Integer;
		Pos : Pos_Vector.Vector;
	end record;

	function "="(n1, n2: in Num_Pos) return Boolean;

	package Num_Vector is new Ada.Containers.Vectors
		(Element_Type => Num_Pos, Index_Type => Natural);

	function To_String(p: in Pos_Vector.Vector) return String;

	procedure To_Num_Pos(a: in Atoms.Atom; thepos: in Pos;
		posvector: in out Pos_Vector.vector; vec: in out Num_Vector.Vector);

	function "+"(n1, n2: in Num_Vector.Vector) return Num_Vector.Vector;
	function "+"(n1: in Num_Vector.Vector; atom: in Atoms.Atom) return Num_Vector.Vector;
	function "+"(atom: in Atoms.Atom; n1: in Num_Vector.Vector) return Num_Vector.Vector;

	function explode(vec: in out Num_Vector.Vector) return Boolean;
	procedure explode(vec: in out Num_Vector.Vector);
	procedure check_vecs(vec: in Num_Vector.Vector);

	function readjust(vec: in out Num_Vector.Vector) return Boolean;

	function split(vec: in out Num_Vector.Vector) return Boolean;

	private
	package pv renames Pos_Vector;
	package nv renames Num_Vector;
	package tio renames Ada.Text_IO;

end Num_Pos_Repr;
