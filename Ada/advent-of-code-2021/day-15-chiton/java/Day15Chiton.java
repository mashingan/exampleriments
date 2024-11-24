import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

class CoordCost implements Comparable<CoordCost>, Addable<CoordCost>, Copyable<CoordCost> {
	int value;
	public int compareTo(CoordCost other) {
		return Integer.compare(value, other.value);
	}

	CoordCost(int value) {
		this.value = value;
	}

	public void addFrom(CoordCost other) {
		this.value += other.value;
	}

	public String toString() {
		return String.format("%3d", value);
	}

	public CoordCost copy() {
		return new CoordCost(value);
	}

	static final CoordCost zeroCost = new CoordCost(0);
}

class Coord implements Cycleable, Comparable<Coord>, Costable<Coord, CoordCost> {
	int x, y;
	CoordCost risk;
	public Coord(int x, int y, CoordCost risk) {
		this.x = x;
		this.y = y;
		this.risk = risk;
	}

	public CoordCost zeroCost() { return new CoordCost(0); }
	public CoordCost cost(Coord otherVertex) {
		return otherVertex.risk;
	}
	public boolean isCycle() { return false; }

	public int compareTo(Coord other) {
		if (x == other.x && y == other.y && risk.compareTo(other.risk) == 0) {
			return 0;
		} else if (x < other.x || y < other.y || risk.compareTo(other.risk) < 0) {
			return -1;
		} else {
			return 1;
		}
	}

	public CoordCost distanceTo(Coord targetVertex) {
		return new CoordCost(
				Math.abs(x - targetVertex.x) +
				Math.abs(y - targetVertex.y));
	}

	public String toString() {
		return String.format("((%2d, %2d) => %2d)", x, y, risk.value);
	}
}

public class Day15Chiton {
	public static void main(String[] args) {
		var maplist = new ArrayList<ArrayList<Coord>>();
		var buf = new BufferedReader(new InputStreamReader(System.in));
		var atomicj = new AtomicInteger(1);
		var atomici = new AtomicInteger(1);
		var graph = new Graph<Coord, CoordCost>();
		try {
			while(true) {
				var s = buf.readLine();
				if (s == null || s.isEmpty()) break;
				var row = s.
					chars().
					mapToObj(c -> new Coord(
								atomici.get(),
								atomicj.getAndIncrement(),
								new CoordCost(c - '0'))).
					collect(Collectors.toCollection(ArrayList<Coord>::new));
				atomicj.set(1);
				atomici.incrementAndGet();
				maplist.add(row);
			}
		} catch(Exception e) {
			e.printStackTrace();
		}
		for (var i = 0; i < maplist.size(); i++) {
			var row = maplist.get(i);
			for (var j = 0; j < row.size(); j++) {
				System.out.print(row.get(j).risk.value);
			}
			System.out.println();
		}

		populateGraph(graph, maplist);
		/*
		for (var node : graph.nodes.toArray(Coord[]::new)) {
			System.out.println(node);
		}
		*/

		System.out.println("edges length: " + graph.edges.size());
		var count = 1;
		for (var edge : graph.edges.toArray(Graph.Edge[]::new)) {
			System.out.printf("edge %3d %s\n", count++, edge);
		}

		System.out.println();
		var start = maplist.get(0).get(0);
		var end = maplist.get(maplist.size()-1).get(maplist.get(0).size()-1);
		var paths = graph.AStar(start, end);
		System.out.println("\ntotal path nodes: " + paths.size());
		System.out.println("AStar:");
		for (var node : paths.toArray(Coord[]::new)) {
			System.out.println(node);
		}
		var total = 0;
		for (var i = 1; i < paths.size(); i++) {
			total += paths.get(i).risk.value;
		}
		System.out.println("Total risk: " + total);
	}

	static void populateGraph(Graph<Coord, CoordCost> graph,
			ArrayList<ArrayList<Coord>> maplist)
	{
		var colsize = maplist.size();
		for (var y = 0; y < colsize; y++) {
			var rowsize = maplist.get(y).size();
			for (var x = 0; x < rowsize; x++) {
				graph.addVertex(maplist.get(y).get(x));
				if (x - 1 >= 0) {
					graph.addVertex(maplist.get(y).get(x-1));
					graph.addEdge(maplist.get(y).get(x), maplist.get(y).get(x-1));
				}
				if (x + 1 < rowsize) {
					graph.addVertex(maplist.get(y).get(x+1));
					graph.addEdge(maplist.get(y).get(x), maplist.get(y).get(x+1));
				}

				if (y - 1 >= 0) {
					graph.addVertex(maplist.get(y-1).get(x));
					graph.addEdge(maplist.get(y).get(x), maplist.get(y-1).get(x));
				}
				if (y + 1 < rowsize) {
					graph.addVertex(maplist.get(y+1).get(x));
					graph.addEdge(maplist.get(y).get(x), maplist.get(y+1).get(x));
				}
			}
		}
	}
}
