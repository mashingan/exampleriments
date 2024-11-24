with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers;
with Ada.Containers.Generic_Array_Sort;
with Ada.Numerics.Elementary_Functions;

package body statistics is
	function Mean (The_Data: Data) return Float is
		Result : Float := 0.0;
		elem : Float := 0.0;
	begin
		for I in The_Data'range loop
			elem := elem + 1.0;
			Result := The_Data(I);
		end loop;
		return Result / elem;
	end Mean;

	function Mode (The_Data: Data) return Float is
		--function Hash(Key: Float) return Float is
		--begin
			--return Key;
		--end Hash;
		--package Float_Hash_map is new Ada.Containers.Indefinite_Hashed_Maps
		--(
			--Key_Type => Float,
			--Element_Type => Integer,
			--Hash => Hash,
			--Equivalent_Keys => "=");
--
		--package fmap renames Float_Hash_map;
		--M : fmap.Map;
		--Count : Integer;
		--Max : Integer := 0;
		Mode_Result : Float := 0.0;
	begin
		--for I in The_Data'range loop
			--if M.Contains(The_Data(I)) then
				--Count := M(The_Data(I));
				--Count := Count + 1;
				--M.Insert(The_Data(I), Count);
			--else
				--M.Insert(The_Data(I), 1);
			--end if;
		--end loop;
--
		--for K in M.iterate loop
			--Count := M(K);
			--if Count > Max then
				--Max := Count;
				--Mode_Result := fmap.Key(K);
			--end if;
		--end loop;
		return Mode_Result;
	end Mode;

	procedure Data_Sort is new Ada.Containers.Generic_Array_Sort
	(
		Index_Type => Positive,
		Element_Type => Float,
		Array_Type => Data);
	function Max  (The_Data: Data) return Float is
		Temp_Data : Data := The_Data;
	begin
		Data_Sort(Temp_Data);
		return Temp_Data(Temp_Data'last);
	end Max;
	function Min  (The_Data: Data) return Float is
		Temp_Data : Data := The_Data;
	begin
		Data_Sort(Temp_Data);
		return Temp_Data(Temp_Data'first);
	end Min;

	function Variance (The_Data: Data) return Float is
		Avg : Float := Mean(The_Data);
		Acc : Float := 0.0;
		Count : Float := 0.0;
	begin
		for I in The_Data'range loop
			Acc := Acc + (The_Data(I) - Avg) ** 2;
			Count := Count + 1.0;
		end loop;
		return Acc / Count;
	end Variance;
	function StdDev (The_Data: Data) return Float is
		Result : Float := Variance(The_Data);
	begin
		return Ada.Numerics.Elementary_Functions.Sqrt(Result);
	end StdDev;
end statistics;

