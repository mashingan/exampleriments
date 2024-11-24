with Ada.Containers.Vectors;
with Ada.Containers;

generic
type Node_Type is private;
type Index_Type is range <>;
with function "=" (Left, Right: in Node_Type)
return Boolean is <>;
with function Is_Cycle(V: in Node_Type)
return Boolean is <>;
with function To_String(V: in Node_Type)
return String is <>;
with function Hash(V: in Node_Type)
return Ada.Containers.Hash_Type is <>;

package Graphs is
	type Edge is record
		Node1, Node2: Node_Type;
	end record;
	function "="(E1, E2: in Edge) return Boolean;

	package Nodes_vector is new Ada.Containers.Vectors
		(Element_Type => Node_Type, Index_Type => Index_Type, "=" => "=");
	package nv renames Nodes_vector;

	package Edges_vector is new Ada.Containers.Vectors
		(Element_Type => Edge, Index_Type => Index_Type, "=" => "=");
	package ev renames Edges_vector;

	package Paths_Vector is new Ada.Containers.Vectors
		(Element_Type => nv.Vector, Index_Type => Index_Type, "=" => nv."=");

	type Graph is record
		nodes: nv.Vector;
		edges: ev.Vector;
	end record;

	function Nodes(g: in Graph) return nv.Vector;
	function Edges(g: in Graph) return ev.Vector;

	procedure Add(g: in out Graph; V: in Node_Type);
	procedure Add(g: in out Graph; E: in Edge);

	function Contains(g: in Graph; V: in Node_Type) return Boolean;
	function Contains(g: in Graph; E: in Edge) return Boolean;
	function Contains(E: in Edge; V: in Node_Type) return Boolean;

	function Neighbors(g: in Graph; V: in Node_Type) return nv.Vector;

	function Paths(g: in Graph; V_Start, V_End: in Node_Type)
		return Paths_Vector.Vector;

	generic
	type Cost_Type is private;
	with function Cost(V1, V2: in Node_Type) return Cost_Type is <>;
	with function "+"(C1, C2: in Cost_Type) return Cost_Type is <>;
	with function "<"(C1, C2: in Cost_Type) return Boolean is <>;
	with function Zero_Cost return Cost_Type is <>;
	function Uniform_Search(g: in Graph; V_Start, V_End: in Node_Type)
		return nv.Vector;

	generic
	type Cost_Type is private;
	with function Distance(Goal, Point: in Node_Type) return Cost_Type is <>;
	with function "<"(D1, D2: in Cost_Type) return Boolean is <>;
	with function Zero_Cost return Cost_Type is <>;
	function Greedy_Best_Search(g: in Graph; V_Start, V_End: in Node_Type)
		return nv.Vector;

	generic
	type Cost_Type is private;
	with function Cost(V1, V2: in Node_Type) return Cost_Type is <>;
	with function "+"(C1, C2: in Cost_Type) return Cost_Type is <>;
	with function "<"(C1, C2: in Cost_Type) return Boolean is <>;
	with function Zero_Cost return Cost_Type is <>;
	with function Distance(Goal, Point: in Node_Type) return Cost_Type is <>;
	function A_Star_Search(g: in Graph; V_Start, V_End: in Node_Type)
		return nv.Vector;

	function Breadth_Search (g: in Graph; V_Start, V_End: in Node_Type)
		return nv.Vector;

	private

	function Neighbor(E: in Edge; V: in Node_Type) return Node_Type;

	procedure Trail(Vs: nv.Vector; prompt : String := "");
end Graphs;
