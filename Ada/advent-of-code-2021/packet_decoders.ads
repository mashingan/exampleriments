with Ada.Text_IO;
with Ada.Containers.Vectors;

package Packet_Decoders is
	package tio renames Ada.Text_IO;

	type Hexadecimal is ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'A', 'B', 'C', 'D', 'E', 'F');

	type Version_Bits is range 0 .. 2;
	type Type_Bits is range 3 .. 5;

	package eio is new Ada.Text_IO.Enumeration_IO (Enum => Hexadecimal);

	function To_Hexadecimal(c: Character) return Hexadecimal;

	function To_Integer(h: Hexadecimal) return Integer;

	package Bits_Vector is new Ada.Containers.Vectors
		(Element_Type => Boolean, Index_Type => Natural);
	package bv renames Bits_Vector;

	function To_Bits(i: Integer) return bv.Vector;

	function To_Bits(h: in Hexadecimal) return bv.Vector;

	function To_Bits(c: in Character) return bv.Vector;

	function To_Integer(b: in bv.Vector) return Integer;

	procedure Check_bits(b: in bv.Vector);

	function Packet_Bits(s: string) return bv.Vector;

	function Read_Version_Type(C: in out bv.Cursor) return Integer;

	function Decode_Literal_bits(C: in out bv.Cursor; buf : in out bv.Vector) return Boolean;

	function Decode_Literal(C: in out bv.Cursor) return Integer;

	function Decode_Operator_0(C: in out bv.Cursor) return Integer;

	function Decode_Operator_1(C: in out bv.Cursor) return Integer;

	function Decode_Packet(cur: in out bv.Cursor) return Integer;

	function Decode_Packet(s: string) return Integer;
end Packet_Decoders;
