with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;

package body graphs is
	package tio renames Ada.Text_IO;
	function "="(E1, E2: in Edge) return Boolean is
		((E1.Node1 = E2.Node1 and E1.Node2 = E2.Node2) or
		(E1.Node1 = E2.Node2 and E1.Node2 = E2.Node1));

	function Nodes(g: in Graph) return nv.Vector is (g.nodes);
	function Edges(g: in Graph) return ev.Vector is (g.edges);

	procedure Add(g: in out Graph; V: in Node_Type) is
	begin
		if g.nodes.Contains(V) then return; end if;
		g.nodes.Append(V);
	end Add;

	procedure Add(g: in out Graph; E: in Edge) is
	begin
		if g.edges.Contains(E) then return; end if;
		g.edges.Append(E);
	end Add;

	function Contains(g: in Graph; V: in Node_Type) return Boolean is
		(g.nodes.Contains(V));
	function Contains(g: in Graph; E: in Edge) return Boolean is
		(g.edges.Contains(E));
	function Contains(E: in Edge; V: in Node_Type) return Boolean is
		(V = E.Node1 or V = E.Node2);

	function Neighbors(g: in Graph; V: in Node_Type) return nv.Vector is
		acc: nv.Vector;
	begin
		for I in g.edges.First_Index .. g.edges.Last_Index loop
			if Contains(g.edges(I), V) then
				acc.Append(neighbor(g.edges(I), V));
			end if;
		end loop;
		return acc;
	end Neighbors;

	function Neighbor(E: in Edge; V: in Node_Type) return Node_Type is
	begin
		if V = E.Node1 then
			return E.Node2;
		else
			return E.Node1;
		end if;
	end Neighbor;

	procedure Trail(Vs: in nv.Vector; prompt: String := "") is
	begin
		if prompt /= "" then
			tio.Put_Line(prompt);
		end if;
		tio.Put("    ");
		for I in Vs.First_Index .. Vs.Last_Index loop
			tio.Put(To_String(Vs(I)) & ", ");
		end loop;
		tio.New_Line;
	end Trail;

	function Paths(g: in Graph; V_start, V_end: in Node_Type)
		return Paths_Vector.Vector is
		result: Paths_Vector.Vector;

		function In_Path(Goal, V: in Node_Type; state: in out nv.Vector) return Boolean is
			nextbound : nv.Vector;
			visiting : Node_Type;
		begin
			state.Append(v);
			if V = Goal then
				if not result.Contains(state) then
					result.Append(state);
				end if;
				state.Delete_Last;
				return true;
			end if;
			nextbound := Neighbors(g, V);
			for I in nextbound.First_Index .. nextbound.Last_Index loop
				visiting := nextbound(I);
				if visiting /= V and (not state.Contains(visiting) or Is_Cycle(visiting)) then
					if not In_Path(Goal, visiting, state)  then
						state.Delete_Last;
					end if;
				end if;
			end loop;
			return false;
		end In_Path;

		Unused: Boolean;
		state: nv.Vector;
	begin
		if not Contains(g, V_start) or not Contains(g, V_end) then return result; end if;
		Unused := In_Path(V_end, V_start, state);
		return result;
	end Paths;


	package Node_Maps is new Ada.Containers.Hashed_Maps
		(Element_Type => Node_Type, Key_Type => Node_Type,
		Hash => Hash, Equivalent_Keys => "=");
	package nm renames Node_Maps;

	function Traceback (V_start, V_end: in Node_Type; path : in nm.Map) return nv.Vector is
		result : nv.Vector := nv.Empty_Vector;
		current : Node_Type;
	begin
		current := V_end;
		loop
			result.Append(current);
			exit when current = V_start;
			if not path.Contains(current) then return nv.Empty_Vector; end if;
			current := path(current);
		end loop;
		result.Reverse_Elements;
		return result;
	end Traceback;

	function Breadth_Search(g: in Graph; V_start, V_end: in Node_Type)
		return nv.Vector is

		visited: nm.Map := nm.Empty_Map;
		visiting: nv.Vector := nv.Empty_Vector;
		next_visit: nv.Vector := nv.Empty_Vector;
		node, next_node: Node_Type;
		found_goal: Boolean := false;

		procedure Traverse_Path is
		begin
			visited.Include(V_start, V_start);
			visiting.Append(V_start);
			loop
				exit when Integer(visiting.Length) = 0;
				node := visiting.First_Element;
				visiting.Delete_First;
				next_visit := Neighbors(g, node);
				for N in next_visit.Iterate loop
					next_node := next_visit(N);
					if next_node = V_end then found_goal := true; end if;
					if not visited.Contains(next_node) or Is_Cycle(next_node) then
						visiting.Append(next_node);
						visited.Include(next_node, node);
					end if;
					exit when found_goal;
				end loop;
				exit when found_goal;
			end loop;
		end Traverse_Path;

	begin
		if not Contains(g, V_start) or not Contains(g, V_end) then
			return nv.Empty_Vector;
		end if;
		Traverse_Path;
		return Traceback(V_start, V_end, visited);
	end Breadth_Search;

	function Uniform_Search(g: in Graph; V_start, V_end: in Node_Type)
		return nv.Vector is

		type Node_Priority_Cost is record
			node: Node_Type;
			cost: Cost_Type;
		end record;

		function Get_Priority(el: Node_Priority_Cost) return Cost_Type is
			(el.cost);

		package Node_Priority_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
			(Element_Type => Node_Priority_Cost);
		package Node_Priority is new Ada.Containers.Unbounded_Priority_Queues
			(Queue_Interfaces => Node_Priority_Interface, Queue_Priority => Cost_Type,
			Before => "<");
		package np renames Node_Priority;

		package Node_Cost is new Ada.Containers.Hashed_Maps
			(Key_Type => Node_Type, Element_Type => Cost_Type, Hash => Hash,
			Equivalent_Keys => "=");
		package nc renames Node_Cost;

		visited: nm.Map := nm.Empty_Map;
		visiting: np.Queue;
		next_visit: nv.Vector := nv.Empty_Vector;
		node, next_node: Node_Type;
		cost_so_far: nc.Map := nc.Empty_Map;
		found_goal: Boolean := false;
		new_cost : Cost_Type := Zero_Cost;
		node_cost_of_priority : Node_Priority_Cost;

	begin
		if not Contains(g, V_start) or not Contains(g, V_end) then
			return nv.Empty_Vector;
		end if;
		cost_so_far.Include(V_start, Zero_Cost);
		visited.Include(V_start, V_start);
		visiting.Enqueue((V_start, Zero_Cost));
		loop select
			visiting.Dequeue(node_cost_of_priority);
			node := node_cost_of_priority.node;
			exit when node = V_end;
			next_visit := Neighbors(g, node);
			for N in next_visit.Iterate loop
				next_node := next_visit(N);
				if cost_so_far.Contains(node) then
					new_cost := cost_so_far(node);
				end if;
				new_cost := new_cost + Cost(node, next_node);
				--tio.Put_Line("new_cost:" & new_cost'image);
				if not visited.Contains(next_node) then
					if not cost_so_far.Contains(next_node) then
						cost_so_far.Include(next_node, new_cost);
					elsif cost_so_far.Contains(next_node) and new_cost < cost_so_far(next_node) then
						cost_so_far(next_node) := new_cost;
					end if;
					visiting.Enqueue((next_node, new_cost));
					visited.Include(next_node, node);
				end if;
			end loop;
		else exit;
		end select;
		end loop;
		return Traceback(V_start, V_end, visited);
	end Uniform_Search;

	function Greedy_Best_Search(g: in Graph; V_start, V_end: in Node_Type)
		return nv.Vector is

		type Node_Priority_Cost is record
			node: Node_Type;
			cost: Cost_Type;
		end record;

		function Get_Priority(el: Node_Priority_Cost) return Cost_Type is
			(el.cost);

		package Node_Priority_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
			(Element_Type => Node_Priority_Cost);
		package Node_Priority is new Ada.Containers.Unbounded_Priority_Queues
			(Queue_Interfaces => Node_Priority_Interface, Queue_Priority => Cost_Type,
			Before => "<");
		package np renames Node_Priority;

		visited: nm.Map := nm.Empty_Map;
		visiting: np.Queue;
		next_visit: nv.Vector := nv.Empty_Vector;
		node, next_node: Node_Type;
		found_goal: Boolean := false;
		new_cost : Cost_Type := Zero_Cost;
		node_cost_of_priority : Node_Priority_Cost;

	begin
		if not Contains(g, V_start) or not Contains(g, V_end) then
			return nv.Empty_Vector;
		end if;
		visited.Include(V_start, V_start);
		visiting.Enqueue((V_start, Zero_Cost));
		loop select
			visiting.Dequeue(node_cost_of_priority);
			node := node_cost_of_priority.node;
			exit when node = V_end;
			next_visit := Neighbors(g, node);
			for N in next_visit.Iterate loop
				next_node := next_visit(N);
				--tio.Put_Line("new_cost:" & new_cost'image);
				if not visited.Contains(next_node) then
					new_cost := Distance(V_end, next_node);
					visiting.Enqueue((next_node, new_cost));
					visited.Include(next_node, node);
				end if;
			end loop;
		else exit;
		end select;
		end loop;
		return Traceback(V_start, V_end, visited);
	end Greedy_Best_Search;

	function A_Star_Search(g: in Graph; V_start, V_end: in Node_Type)
		return nv.Vector is

		type Node_Priority_Cost is record
			node: Node_Type;
			cost: Cost_Type;
		end record;

		function Get_Priority(el: Node_Priority_Cost) return Cost_Type is
			(el.cost);

		package Node_Priority_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
			(Element_Type => Node_Priority_Cost);
		package Node_Priority is new Ada.Containers.Unbounded_Priority_Queues
			(Queue_Interfaces => Node_Priority_Interface, Queue_Priority => Cost_Type,
			Before => "<");
		package np renames Node_Priority;

		package Node_Cost is new Ada.Containers.Hashed_Maps
			(Key_Type => Node_Type, Element_Type => Cost_Type, Hash => Hash,
			Equivalent_Keys => "=");
		package nc renames Node_Cost;

		visited: nm.Map := nm.Empty_Map;
		visiting: np.Queue;
		next_visit: nv.Vector := nv.Empty_Vector;
		node, next_node: Node_Type;
		cost_so_far: nc.Map := nc.Empty_Map;
		found_goal: Boolean := false;
		new_cost : Cost_Type := Zero_Cost;
		node_cost_of_priority : Node_Priority_Cost;
		priority : Cost_Type := Zero_Cost;

	begin
		if not Contains(g, V_start) or not Contains(g, V_end) then
			return nv.Empty_Vector;
		end if;
		cost_so_far.Include(V_start, Zero_Cost);
		visited.Include(V_start, V_start);
		visiting.Enqueue((V_start, Zero_Cost));
		loop
			exit when Integer(visiting.Current_Use) = 0;
			visiting.Dequeue(node_cost_of_priority);
			node := node_cost_of_priority.node;
			exit when node = V_end;
			next_visit := Neighbors(g, node);
			for N in next_visit.Iterate loop
				next_node := next_visit(N);
				if cost_so_far.Contains(node) then
					new_cost := cost_so_far(node);
				end if;
				new_cost := new_cost + Cost(node, next_node);
				--tio.Put_Line("next visit:" & To_String(next_node) & ", new_cost:" & Integer'image(new_cost));
				if not visited.Contains(next_node) then
					if not cost_so_far.Contains(next_node) then
						cost_so_far.Include(next_node, new_cost);
					elsif cost_so_far.Contains(next_node) and new_cost < cost_so_far(next_node) then
						cost_so_far(next_node) := new_cost;
					end if;
					priority := new_cost + Distance(V_end, next_node);
					visiting.Enqueue((next_node, priority));
					visited.Include(next_node, node);
				end if;
			end loop;
		end loop;
		return Traceback(V_start, V_end, visited);
	end A_Star_Search;
end graphs;
