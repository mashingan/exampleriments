import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.PriorityQueue;
import java.util.stream.Collectors;

interface Cycleable {
	boolean isCycle();
}

interface Addable<C> {
	void addFrom(C other);
}

interface Copyable<C> {
	C copy();
}

interface Costable<V, C extends Comparable<C>> {
	C zeroCost();
	C cost(V otherVertex);
	C distanceTo(V targetVertex);
}

public class Graph<
	V extends Comparable<V> & Cycleable & Costable<V, C>,
	C extends Comparable<C> & Addable<C> & Copyable<C>>
{
	public class Edge {
		V node1, node2;
		public String toString() {
			return String.format("%s -> %s", node1, node2);
		}

		public Edge(V n1, V n2) {
			node1 = n1;
			node2 = n2;
		}
	}

	public ArrayList<V> nodes = new ArrayList<>();
	public ArrayList<Edge> edges = new ArrayList<>();
	HashMap<V, ArrayList<V>> internalEdges = new HashMap<>();

	public void addVertex(V v) {
		if (nodes.contains(v)) return;
		nodes.add(v);
	}

	public void addEdge(V v1, V v2) {
		if (!nodes.contains(v1)) nodes.add(v1);
		if (!nodes.contains(v2)) nodes.add(v2);
		if (internalEdges.containsKey(v1) && internalEdges.get(v1).contains(v2)) {
			return;
		}
		if (internalEdges.containsKey(v2) && internalEdges.get(v2).contains(v1)) {
			return;
		}
		var v1v2 = internalEdges.getOrDefault(v1, new ArrayList<>());
		var v2v1 = internalEdges.getOrDefault(v2, new ArrayList<>());
		v1v2.add(v2);
		v2v1.add(v1);
		internalEdges.put(v1, v1v2);
		internalEdges.put(v2, v2v1);
		var e = new Edge(v1, v2);
		edges.add(e);
	}

	public void addVertices(V[] vs) {
		for (var v : vs) {
			nodes.add(v);
		}
	}

	public boolean contains(V v) {
		return nodes.contains(v);
	}

	public boolean contains(Edge e) {
		return edges.contains(e);
	}

	public void addEdges(Edge[] es) {
		for (var e : es) {
			addEdge(e.node1, e.node2);
		}
	}

	public ArrayList<V> neighbors(V node) {
		var neighbors = new ArrayList<V>();
		for (var e : edges) {
			if (e.node1 == node) {
				neighbors.add(e.node2);
			} else if (e.node2 == node) {
				neighbors.add(e.node1);
			}
		}
		return neighbors;
	}

	public ArrayList<ArrayList<V>> paths(V start, V end) {
		if (!nodes.contains(start) || !nodes.contains(end)) {
			return null;
		}
		var state = new ArrayDeque<V>();
		ArrayList<ArrayList<V>> res = new ArrayList<>();
		inPath(end, start, state, res);
		return res;
	}

	boolean inPath(V goal, V node, ArrayDeque<V> state, ArrayList<ArrayList<V>> acc) {
		state.add(node);
		//System.out.println("visiting: " + node.toString());
		if (node == goal) {
			//System.out.println("node is same with goal");
			var thelist = state.stream().collect(Collectors.toCollection(ArrayList<V>::new));
			if (!acc.contains(thelist)) {
				acc.add(thelist);
			}
			state.removeLast();
			return true;
		}
		var nextbound = neighbors(node);
		nextbound.stream().forEach(v -> {
			//System.out.printf("neighbors of node (%s) is v (%s)\n" , node, v);
			if (v != node && (!state.contains(v) || v.isCycle())) {
				if (!inPath(goal, v, state, acc)) {
					state.removeLast();
				}
			} else {
				//System.out.printf("Cannot visit neighbor (%s)\n", v);
			}
		});
		return false;
	}

	class NodePriority {
		V node;
		C cost;

		NodePriority(V node, C cost) {
			this.node = node;
			this.cost = cost;
		}
	}

	ArrayList<V> traceback(V start, V end, HashMap<V, V> path) {
		var retpath = new ArrayList<V>();
		var current = end;
		while(true) {
			retpath.add(0, current);
			if (current == start) break;
			if (!path.containsKey(current)) {
				return new ArrayList<>();
			}
			current = path.get(current);
		}
		return retpath;
	}

	public ArrayList<V> AStar (V start, V end) {
		if (!nodes.contains(start) || !nodes.contains(end)) {
			return new ArrayList<>();
		}
		var visited = new HashMap<V, V>();
		var costSoFar = new HashMap<V, C>();
		var visiting = new PriorityQueue<NodePriority>(
				(n1, n2) -> n1.cost.compareTo(n2.cost));
		visited.put(start, start);
		visiting.add(new NodePriority(start, start.zeroCost()));
		C newcost = start.zeroCost();
		costSoFar.put(start, start.zeroCost());
		while (true) {
			if (visiting.size() == 0) {
				break;
			}
			var v = visiting.poll();
			if (v.node == end) {
				break;
			}
			//System.out.printf("from node %s with priority %s\n", v.node, v.cost);
			var nextvisit = neighbors(v.node);
			for (var i = 0; i < nextvisit.size(); i++) {
				var nextnode = nextvisit.get(i);
				//System.out.printf("\tvisiting node %s", nextnode);
				newcost = costSoFar.get(v.node).copy();
				newcost.addFrom(v.node.cost(nextnode));
				//System.out.printf(" with current cost %s\n", newcost);
				if (!costSoFar.containsKey(nextnode) || newcost.compareTo(costSoFar.get(nextnode)) < 0) {
					costSoFar.put(nextnode, newcost);
					var priority = newcost.copy();
					priority.addFrom(nextnode.distanceTo(end));
					//System.out.printf("\tAdding node %s with priority %s and cost %s\n",
							//nextnode, priority, newcost);
					visiting.add(new NodePriority(nextnode, priority));
					visited.put(nextnode, v.node);
				}
			}
		}
		return traceback(start, end, visited);
	}
}
