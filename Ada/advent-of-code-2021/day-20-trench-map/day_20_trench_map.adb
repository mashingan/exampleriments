-- compile it: (small)
-- gnatmake -O2 -I.. day_20_trench_map.adb -bargs -largs -s
-- (normal)
-- gnatmake day_20_trench_map.adb
-- run in cmd (not powershell)
-- type input.txt | day_20_trench_map.exe
-- or simply run:
-- build-run.bat
-- to clean folder: adaclean

with Ada.Text_IO;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with packet_decoders;

procedure day_20_trench_map is
	package tio renames Ada.Text_IO;
	package fix renames Ada.Strings.Fixed;
	package pd renames packet_decoders;

	package Bits_Vectors is new Ada.Containers.Vectors
		(Element_Type => pd.bv.Vector, Index_Type => Natural, "=" => pd.bv."=");
	package bbv renames Bits_Vectors;

	function Dot(c: in Character) return Boolean is
		(c = '#');

	type Map is record
		size: Natural;
		dots: bbv.Vector := bbv.Empty_Vector;
	end record;

	Initial_map : Map;
	Enhancement_Algoritm : String(1..512);

	procedure Add_Edges(the_map : in out Map) is
		use Ada.Containers;
		bitsline : pd.bv.Vector := pd.bv.Empty_Vector;
		size : Count_Type;
	begin
		size := Count_Type(pd.bv.Length (the_map.dots.First_Element));
		for I in 1 .. size loop
			bitsline.Append (false);
		end loop;
		the_map.dots.Append (bitsline);
		bitsline := pd.bv.Empty_Vector;
		for I in 1 .. size loop
			bitsline.Append (false);
		end loop;
		the_map.dots.Prepend (bitsline);
	end Add_Edges;

	procedure Read_input is
		length : Natural := 0;
		line : String(1..40);
		bitsline : pd.bv.Vector := pd.bv.Empty_Vector;
		size : Natural := 0;
	begin
		Initial_map.size := 0;
		tio.Get_Line(Enhancement_Algoritm, length);
		for I in 1..2 loop tio.Get_Line(line, length); end loop; -- discard rest and an empty line
		loop
			begin
				tio.Get_Line(line, length);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			if size = 0 then
				size := length + 2;
			end if;
			for I in line'first .. length loop
				bitsline.Append(Dot(line(I)));
			end loop;
			bitsline.Append(false);
			bitsline.Prepend(false);
			Initial_map.dots.Append (bitsline);
			bitsline := pd.bv.Empty_Vector;
		end loop;
		Add_Edges (Initial_map);
	end Read_input;

	procedure draw_map(the_map: in Map) is
	begin
		for vec of the_map.dots loop
			for mark of vec loop
				tio.Put(if mark then '#' else '.');
				--tio.Put(' ');
			end loop;
			tio.New_Line;
		end loop;
	end draw_map;

	function apply_enhancement(input: in Map) return Map is
		use Ada.Containers;
		use pd.bv;
		resmap : Map;
		vec : pd.bv.Vector := pd.bv.Empty_Vector;
		bit_cruncher : pd.bv.Vector := pd.bv.Empty_Vector;
		tempvec : pd.bv.Vector := pd.bv.Empty_Vector;
		newvec : pd.bv.Vector := pd.bv.Empty_Vector;
		index_pos : Integer := 0;
		veclen : Natural := 0;
	begin
		resmap.dots := bbv.Empty_Vector;
		for I in input.dots.First_Index .. input.dots.Last_Index loop
			vec := input.dots(I);
			for J in vec.First_Index .. vec.Last_Index loop
				veclen := Natural(vec.Length) + 2;
				if I-1 >= input.dots.First_Index then
					tempvec := false & input.dots(I-1) & false;
				else
					tempvec := To_Vector (false, Count_Type(veclen));
				end if;
				for K in J .. J+2 loop
					pd.bv.Append(bit_cruncher, tempvec(K));
				end loop;
				tempvec := false & vec & false;
				for K in J .. J+2 loop
					pd.bv.Append(bit_cruncher, tempvec(K));
				end loop;
				if I+1 <= input.dots.Last_Index then
					tempvec := false & input.dots(I+1) & false;
				else
					tempvec := To_Vector (false, Count_Type(veclen));
				end if;
				for K in J .. J+2 loop
					pd.bv.Append(bit_cruncher, tempvec(K));
				end loop;
				index_pos := pd.To_Integer(bit_cruncher) + 1;
				--tio.Put_Line("(X, Y): (" & J'image & ',' & I'image);
				--tio.Put_Line("index_pos:" & index_pos'image);
				--pd.Check_bits(bit_cruncher);
				newvec.Append(Dot(Enhancement_Algoritm(index_pos)));
				bit_cruncher.Clear;
			end loop;
			newvec.Append(false);
			newvec.Prepend (false);
			resmap.dots.Append(newvec);
			newvec.Clear;
		end loop;
		Add_Edges (resmap);
		return resmap;
	end apply_enhancement;

	outmap : Map;
	markcount : Natural := 0;
begin
	Read_input;
	tio.Put ("Enhancement_Algoritm: " & Enhancement_Algoritm);
	tio.New_Line;
	tio.Put_Line("Initial map");
	tio.New_Line;
	draw_map (Initial_map);
	tio.New_Line;
	tio.Put_Line("applied 1 map");
	tio.New_Line;
	outmap := apply_enhancement (Initial_map);
	draw_map (outmap);
	tio.New_Line;
	tio.Put_Line("applied 2 map");
	tio.New_Line;
	outmap := apply_enhancement (outmap);
	draw_map (outmap);
	for vec of outmap.dots loop
		for marked of vec loop
			if marked then markcount := markcount + 1; end if;
		end loop;
	end loop;
	tio.Put_Line ("The markcount after twice enhancement is" & markcount'image);
end day_20_trench_map;
