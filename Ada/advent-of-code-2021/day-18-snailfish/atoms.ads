with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Bounded;

package Atoms is
	type Atom;
	type Atom_Access is access all Atom;

	Invalid_Input : exception;

	type Tuple is array (1..2) of Atom_Access;
	type Atom_Discriminant is (Single, Pair);

	type Atom is record
		Parent : Atom_Access := null;
		Kind: Atom_Discriminant := Single;
		Value : Integer := 0;
		Values : Tuple := (null, null);
	end record;

	function "="(A1, A2: in Atom) return Boolean;
	function "+"(A1, A2: in Atom) return Atom;

	function Read_Atom(s: String; last: in out Integer;
		parent : in Atom_Access := null) return Atom;
	function Read_Pair(s: String; last: in out Integer;
		parent : in Atom_Access := null) return Atom;

	function To_String(a: in Atom) return String;
	function Get_Access(a : in Atom) return Atom_Access;
	function explode(a: in out Atom) return Boolean;

	private
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;

	package bs is new Ada.Strings.Bounded.Generic_Bounded_Length
		(Max => 128);

end Atoms;
