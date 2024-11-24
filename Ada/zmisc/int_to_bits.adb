with Ada.Text_IO;
with Ada.Integer_Text_IO;
with System;
procedure int_to_bits is
   package tio renames Ada.Text_IO;
   package nio renames Ada.Integer_Text_IO;

   type Hexadecimal is ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'A', 'B', 'C', 'D', 'E', 'F');


   package eio is new Ada.Text_IO.Enumeration_IO (Enum => Hexadecimal);

   function To_Hexadecimal(c: Character) return Hexadecimal is
      hx : Hexadecimal;
      last : Positive;
   begin
      eio.Get(c'image, hx, last);
      return hx;
   end To_Hexadecimal;

   function To_Integer(h: Hexadecimal) return Integer is (Hexadecimal'pos(h));

   --Hexachar : array (Hexadecimal) of Boolean;


   Half_Word : Constant := 1;
   type Bits_Array is array (1..4) of Boolean;
   type Bits_Int is record
      value : Bits_Array;
   end record;
   --for Bits_Int use record
      --value at 0*Half_Word range 1 .. 4;
   --end record;

   --for Bits_Int'size use 1*System.Storage_Unit;
   for Bits_Int'Alignment use 2;

   char : Character := 'A';
   num : Integer;
   last: Positive;
   hx: Hexadecimal;
begin
   tio.Put_Line(num'size'image);
   nio.Default_Width := 1;
   nio.Default_Base := 16;
   tio.Put_Line("get the num");
   --nio.Get(char & "", num, last);
   --nio.Default_Base := 10;
   --num := integer'value(char & "");
   --tio.Put_Line("success get the num");
   --tio.Put_Line(num'image);
   hx := To_Hexadecimal(char);
   tio.Put_Line(Hexadecimal'pos(hx)'image);
   tio.Put_Line(Hexadecimal'val(10)'image);

   char := '2';
   hx := To_Hexadecimal(char);
   tio.Put_Line(Hexadecimal'pos(hx)'image);
   tio.Put_Line(Hexadecimal'val(2)'image);
end int_to_bits;
