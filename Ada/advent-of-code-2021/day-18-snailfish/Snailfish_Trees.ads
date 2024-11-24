with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;
with Ada.Containers.Multiway_Trees;

package Snailfish_Trees is

	type Tree_Kind is (Leaf, Branch);
	type Tree_element(kind: Tree_Kind := Leaf) is record
		case kind is
			when Leaf => Num : Integer;
			when others => null;
		end case;
	end record;

	package Max120String is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 120);

	function "="(t1, t2: in Tree_element) return Boolean;

	package Snailfish_Trees is new Ada.Containers.Multiway_Trees
		(Element_Type => Tree_element);

	package Tree_Vector is new Ada.Containers.Vectors
		(Element_Type => Snailfish_Trees.Tree, Index_Type => Natural,
		"=" => Snailfish_Trees."=");

	function To_String(c: in out Snailfish_Trees.Cursor) return String;
	function To_String(tree: in Snailfish_Trees.Tree) return String;

	Snailfishes : Tree_Vector.Vector;

	procedure Read_Fish(s: in String; last : in out Integer;
		tree: in out Snailfish_Trees.Tree;
		cur : in out Snailfish_Trees.Cursor);
	procedure Read_Pair_Fish(s: in String; last : in out Integer;
		tree: in out Snailfish_Trees.Tree;
		cur : in out Snailfish_Trees.Cursor);
	function Read_Snailfish(s: in String) return Snailfish_Trees.Tree;
	procedure Read_Snailfishes;

	private
	package tio renames Ada.Text_IO;
	package sft renames Snailfish_Trees;
	package tv renames Tree_Vector;
end Snailfish_Trees;
