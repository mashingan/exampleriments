-- compile it: (small)
-- gnatmake -O2 -I.. Day_15_chiton.adb -bargs -largs -s
-- (normal)
-- gnatmake Day_15_chiton.adb
-- run in cmd (not powershell)
-- type input.txt | Day_15_chiton.exe
-- or simply run:
-- build-run.bat
-- to clean folder: gnatclean
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers;
with Ada.Strings.Hash;
with Ada.Containers.Vectors;
with graphs;

procedure Day_15_chiton is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;

	type Dimension is range 1 .. 10;
	subtype Risk_level is Integer range 1 .. 10;
	type Risk_Map is array(Dimension, Dimension) of Risk_level;
	type Dir is (Up, Left, Down, Right);

	type Coord is record
		x, y: Dimension;
		risk: Risk_level;
	end record;

	function "=" (c1, c2: in Coord) return Boolean is
		(c1.x = c2.x and c1.y = c2.y and c1.risk = c2.risk);

	function Cycle(c: in Coord) return Boolean is (false);

	function To_String(c: in Coord) return String is
		("((" & c.x'image & ',' & c.y'image & ") =>" & c.risk'image & ')');

	function Hash(c: in Coord) return Ada.Containers.Hash_Type is
		(Ada.Strings.Hash(To_String(c)));

	cave: Risk_Map := (others => (others => 1));

	package Move_Graph is new Graphs
		(Index_Type => Positive, Node_Type => Coord, Is_Cycle => Cycle);
	package mg renames Move_Graph;

	cave_graph: mg.Graph;

	procedure Read_Map is
		line: String(1..11);
		length: Natural;
		last: Positive;
	begin
		for I in Dimension loop
			begin
				tio.Get_Line(line, length);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			exit when length = 0 or length > Integer(Dimension'last);
			for J in Dimension loop
				nio.Get(line(Integer(J)) & "", cave(I, J), last);
			end loop;
		end loop;
	end Read_Map;

	procedure Populate_Graph is
		edge: mg.Edge;
		co1, co2: Coord;
	begin
		for I in Dimension loop
			for J in Dimension loop
				co1 := (I, J, cave(I, J));
				mg.Add(cave_graph, co1);
				if Integer(I) - 1 >= Integer(Dimension'first) then
					co2 := (I-1, J, cave(I-1, J));
					edge := (co1, co2);
					mg.Add(cave_graph, co2);
					mg.Add(cave_graph, edge);
				end if;
				if Integer(I) + 1 <= Integer(Dimension'last) then
					co2 := (I+1, J, cave(I+1, J));
					edge := (co1, co2);
					mg.Add(cave_graph, co2);
					mg.Add(cave_graph, edge);
				end if;
				if Integer(J) - 1 >= Integer(Dimension'first) then
					co2 := (I, J-1, cave(I, J-1));
					edge := (co1, co2);
					mg.Add(cave_graph, co2);
					mg.Add(cave_graph, edge);
				end if;
				if Integer(J) + 1 <= Integer(Dimension'last) then
					co2 := (I, J+1, cave(I, J+1));
					edge := (co1, co2);
					mg.Add(cave_graph, co2);
					mg.Add(cave_graph, edge);
				end if;
			end loop;
		end loop;
	end;
	start_Node : Coord;
	end_Node : Coord;
	paths: mg.nv.Vector;

	function Zero_Cost return Integer is (0);
	function Cost(c1, c2: in Coord) return Integer is (c2.risk);
	function Distance(c1, c2: in Coord) return Integer is
		(abs(Integer(c1.x) - Integer(c2.x)) + abs(Integer(c1.y) - Integer(c2.y)));
	function Dijkstra_search is new mg.Uniform_Search
		(Cost_Type => Natural);
	function A_star is new mg.A_Star_Search
		(Cost_Type => Natural);
begin
	Read_Map;
	start_Node := (Dimension'first, Dimension'first,
	cave(Dimension'first, Dimension'first));
	end_Node := (Dimension'last, Dimension'last,
	cave(Dimension'last, Dimension'last));
	Populate_Graph;
	for I in Dimension loop
		for J in Dimension loop
			tio.Put(cave(I, J)'image);
		end loop;
		tio.New_Line;
	end loop;
	for C in cave_graph.nodes.Iterate loop
		tio.Put(To_String(mg.Nodes_vector.Element(C)) & ", ");
	end loop;
	tio.Put_Line("The total nodes are" & Integer(cave_graph.nodes.Length)'image);
	declare
		edge: mg.Edge;
	begin
		for E in cave_graph.edges.Iterate loop
			edge := mg.Edges_vector.Element(E);
			tio.Put_Line(To_String(edge.Node1) & " => " & To_String(edge.Node2));
		end loop;
	end;
	tio.Put_Line ("The total edges are" & Integer(cave_graph.edges.Length)'image);
	tio.Put_Line("A_Star_Search");
	paths := A_Star(cave_graph, start_Node, end_Node);
	declare
		total_cost: Natural := 0;
	begin
		for V in paths.Iterate loop
			tio.Put_Line(To_String(paths(V)));
			if mg.nv.To_Index(V) /= paths.First_Index then
				total_cost := total_cost + paths(V).risk;
			end if;
		end loop;
		tio.Put_Line("The total risk path is" & total_cost'image);

		total_cost := 0;
		tio.New_Line;
		tio.Put_Line("Dijkstra_search");
		paths := Dijkstra_search(cave_graph, start_Node, end_Node);
		for V in paths.Iterate loop
			tio.Put_Line(To_String(paths(V)));
			if mg.nv.To_Index(V) /= paths.First_Index then
				total_cost := total_cost + paths(V).risk;
			end if;
		end loop;
		tio.Put_Line("The total risk path is" & total_cost'image);
	end;
end Day_15_chiton;
