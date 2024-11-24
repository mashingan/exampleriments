with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics;

procedure ifsyntax is
	package num renames Ada.Numerics;
	package tio renames Ada.Text_IO;
	package itio renames Ada.Integer_Text_IO;
	A : Integer;
	B : Integer;
	X : Integer;
begin
	tio.Put("Insert A and B consecutively:");
	itio.Get(A);
	itio.Get(B);
	X := (A * B);
	tio.Put(A'image & " *" & B'image & " is" & X'image);
end ifsyntax;
