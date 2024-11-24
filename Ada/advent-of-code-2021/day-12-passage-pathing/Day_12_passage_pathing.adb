-- compile it:
-- gnatmake -O2 Day_12_passage_pathing.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_12_passage_pathing.exe
-- to clean folder: gnatclean
-- or run (in cmd and powershell):
-- build-run.bat
-- or (in powershell)
-- build-run.ps1
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Containers;
with Ada.Strings.Hash;
with graphs;

procedure Day_12_passage_pathing is
	package tio renames Ada.Text_IO;
	package hdl renames Ada.Characters.Handling;
	package fix renames Ada.Strings.Fixed;
	package str renames Ada.Strings;

	package Label_String is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 10);
	package ls renames Label_String;
	type Node is record
		label: ls.Bounded_String;
		is_large: Boolean := false;
	end record;

	function "="(N1, N2: in Node) return Boolean is
		(ls."="(N1.label, N2.label));

	function To_String(n: in Node) return String is
		buff: ls.Bounded_String := n.label;
	begin
		if n.is_large then buff := ls."&"(buff, '^'); end if;
		return ls.To_String(buff);
	end To_String;

	function Hash(n: in Node) return Ada.Containers.Hash_Type is
		(Ada.Strings.Hash (ls.To_String(n.label)));
	function Large_Enough(N: in Node) return Boolean is (N.is_large);
	package Graphs_Node is new Graphs
		(Index_Type => Positive, Node_Type => Node, "=" => "=",
		Is_Cycle => Large_Enough, To_String => To_String);
	package gn renames Graphs_Node;

	function To_String(e: in gn.Edge) return String is
		('(' & To_String(e.Node1) & " -> " & To_String(e.Node2) & ')');

	res: gn.nv.Vector;
	Dir_map : gn.Graph;
	start_Node, end_Node: Node;

	function Assign_Node(s: String) return Node is
		node1: Node;
	begin
		node1 := (ls.To_Bounded_String(s), true);
		for J in s'first .. s'last loop
			if hdl.Is_Lower(s(J)) then
				node1.is_large := false;
				exit;
			end if;
		end loop;
		return node1;
	end Assign_Node;

	procedure Fetch_Direction is
		line: String(1..20);
		length: Natural;
		node1, node2: Node;
		edge: gn.Edge;
		hypen_idx : Natural := 0;
	begin
		loop
			begin
				tio.Get_Line(line, length);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			hypen_idx := fix.Index(line(1 .. length), "-");
			if hypen_idx /= 0 then
				node1 := Assign_Node(line(1 .. hypen_idx-1));
				node2 := Assign_Node(line(hypen_idx+1 .. length));
				if ls."="(node1.label, "start") then
					start_Node := node1;
				elsif ls."="(node2.label, "start") then
					start_Node := node2;
				elsif ls."="(node1.label, "end") then
					end_Node := node1;
				elsif ls."="(node2.label, "end") then
					end_Node := node2;
				end if;
				edge := (node1, node2);
				gn.Add(Dir_map, node1);
				gn.Add(Dir_map, node2);
				gn.Add(Dir_map, edge);
			end if;
		end loop;
	end Fetch_Direction;

	paths: gn.Paths_Vector.Vector;
begin
	Fetch_Direction;
	for I in Dir_map.nodes.First_Index .. Dir_map.nodes.Last_Index loop
		tio.Put(To_String(Dir_map.nodes(I)) & ',');
	end loop;
	tio.New_Line;
	for I in Dir_map.edges.First_Index .. Dir_map.edges.Last_Index loop
		tio.Put_Line(To_String(Dir_map.edges(I)));
	end loop;
	paths := gn.Paths(Dir_map, start_Node, end_Node);
	for I in paths.First_Index .. paths.Last_Index loop
		tio.Put("Result(" & I'image & "): ");
		for J in paths(I).First_Index .. paths(I).Last_Index loop
			tio.Put(To_String(Paths(I)(J)) & ", ");
		end loop;
		tio.New_Line;
	end loop;
end Day_12_passage_pathing;
