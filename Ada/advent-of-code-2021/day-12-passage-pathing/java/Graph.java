import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.stream.Collectors;

interface Cycleable {
	boolean isCycle();
}

public class Graph<V extends Comparable<V> & Cycleable> {
	public class Edge {
		V node1, node2;
		public String toString() {
			return String.format("(%s -> %s)", node1, node2);
		}
	}

	public ArrayList<V> nodes = new ArrayList<>();
	public ArrayList<Edge> edges = new ArrayList<>();

	public void addVertex(V v) {
		if (nodes.contains(v)) return;
		nodes.add(v);
	}

	public void addEdge(V v1, V v2) {
		if (!nodes.contains(v1)) nodes.add(v1);
		if (!nodes.contains(v2)) nodes.add(v2);
		var e = new Edge();
		e.node1 = v1;
		e.node2 = v2;
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
			edges.add(e);
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
}
