with Ada.Unchecked_Conversion;

procedure Unchecked_Example is
	type Vector is array(1 .. 4) of Integer;
	for Vector'Size use 4 * Integer'Size;
	type Data is record
		V1, V2, V3, V4: Integer;
	end record;
	for Data'size use 4 * integer'size;
	function Convert is new Ada.Unchecked_Conversion
	(Source => Vector, Target => Data);

	The_Vector: Vector := (2, 4, 6, 8);
	The_Data: Data := (1, 3, 5, 7);
begin
	The_Data := Convert(The_Vector);
end Unchecked_Example;
