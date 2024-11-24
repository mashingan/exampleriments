-- compile it:
-- gnatmake -O2 Day_14_extended_polymerization.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_14_extended_polymerization.exe
-- to clean folder: adaclean
with Ada.Text_IO;
with Ada.Strings.Hash;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure Day_14_extended_polymerization is
	package tio renames Ada.Text_IO;

	subtype Chemical_Node is Character range 'A' .. 'Z';
	subtype Chemical_Sequence is String(1..2);
	type Chemical_Occurrence is array(Chemical_Node) of Integer;

	Invalid_Chemical_Node : Exception;

	package Sequence_Formula is new Ada.Containers.Hashed_Maps
		(Element_Type => Chemical_Node, Key_Type => Chemical_Sequence,
		Hash => Ada.Strings.Hash, Equivalent_Keys => "=");
	package sf renames Sequence_Formula;

	package Polymer_Sequence is new Ada.Containers.Vectors
		(Element_Type => Chemical_Node, "=" => "=", Index_Type => Natural);
	package ps renames Polymer_Sequence;

	type Polymer_Template is record
		Sequence : ps.Vector := ps.Empty_Vector;
		Formula : sf.Map := sf.Empty_Map;
		Occurence: Chemical_Occurrence := ('A' .. 'Z' => 0);
	end record;
	The_Template : Polymer_Template;

	procedure Read_Sequence is
		line : String(1..10);
		length: Natural := 0;
		node: Chemical_Node;
		node_occurence: Chemical_Occurrence renames The_Template.Occurence;
	begin
		tio.Get_Line(line, length);
		if length = 0 then raise tio.Data_Error; end if;
		for I in line'first .. length loop
			The_Template.Sequence.Append(line(I));
			if not(line(I) in Chemical_Node) then raise Invalid_Chemical_Node; end if;
			node := line(I);
			node_occurence(node) := node_occurence(node) + 1;
		end loop;
		tio.Get_Line(line, length); -- discard empty line
		if length /= 0 then raise tio.Data_Error; end if;
	end Read_Sequence;

	procedure Read_Formulas is
		line: String(1 .. 10);
		length: Natural;
	begin
		loop
			begin
				tio.Get_Line(line, length);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			exit when length = 0;
			The_Template.Formula.Include(line(1..2), line(length));
		end loop;
	end Read_Formulas;

	procedure Process_Sequence is
		seq: ps.Vector renames The_Template.Sequence;
		formula: sf.Map renames The_Template.Formula;
		occurence: Chemical_Occurrence renames The_Template.Occurence;
		chemseq: Chemical_Sequence;
		newseq: ps.Vector;
		chemnode: Chemical_Node;
	begin
		newseq.Append(seq.First_Element);
		for I in seq.First_Index+1 .. seq.Last_Index loop
			chemseq := seq(I-1) & seq(I);
			if formula.Contains(chemseq) then
				chemnode := formula(chemseq);
				occurence(chemnode) := occurence(chemnode) + 1;
				newseq.Append(chemnode);
				newseq.Append(seq(I));
			else
				newseq.Append(seq(I));
			end if;
		end loop;
		seq := newseq;
	end Process_Sequence;

	procedure Inspect_Template is
	begin
		tio.Put_Line("Sequence:");
		tio.Put("    ");
		for I in The_Template.Sequence.First_Index .. The_Template.Sequence.Last_Index loop
			tio.Put(The_Template.Sequence(I));
		end loop;
		tio.New_Line;
		tio.Put_Line("Occurence:");
		tio.Put("    ");
		for I in The_Template.Occurence'first .. The_Template.Occurence'last loop
			tio.Put(I & ':' & The_Template.Occurence(I)'image & ", ");
		end loop;
	end Inspect_Template;

	procedure Inspect_Formula is
	begin
		tio.New_Line;
		tio.Put_Line("Formulas:");
		for C in The_Template.Formula.Iterate loop
			tio.Put_Line("    " & sf.Key(C) & " -> " & The_Template.Formula(C));
		end loop;
	end Inspect_Formula;

	procedure Max_Min_Occurrence(Max, Min: out Natural) is
		occ : Chemical_Occurrence renames The_Template.Occurence;
	begin
		Max := 0;
		Min := 0;
		for I in occ'first .. occ'last loop
			if Max = 0 then Max := occ(I); end if;
			if Min = 0 then Min := occ(I); end if;
			if occ(I) /= 0 then
				if occ(I) > Max then Max := occ(I); end if;
				if occ(I) < Min then Min := occ(I); end if;
			end if;
		end loop;
	end Max_Min_Occurrence;

	Max_Num, Min_Num: Natural := 0;
begin
	Read_Sequence;
	Read_Formulas;
	Inspect_Template;
	for I in 1 .. 10 loop
		--tio.New_Line;
		--tio.Put_Line("step:" & I'image);
		Process_Sequence;
		--Inspect_Template;
	end loop;
	Max_Min_Occurrence(Max_Num, Min_Num);
	tio.New_Line;
	tio.Put_Line("The diff of max and min occurrence:" & Integer'image(Max_Num - Min_Num));
end Day_14_extended_polymerization;
