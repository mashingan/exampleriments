import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Comparator;
import java.util.HashMap;

class Node implements Comparable<Node>, Cycleable, Comparator<Node> {
	String label;
	boolean largeEnough;

	public boolean isCycle() {
		return largeEnough;
	}

	Node(String label) {
		this.label = label;
		for (var c : label.toCharArray()) {
			if (Character.isLowerCase(c)) {
				largeEnough = false;
				return;
			}
		}
		largeEnough = true;
	}

	public int compareTo(Node n) {
		return label.compareTo(n.label);
	}

	public int compare(Node own, Node n) {
		return own.label.compareTo(n.label);
	}

	public String toString() {
		return String.format("%s%c", label, largeEnough? '^' : ' ');
	}

	public boolean equals(Node n) {
		return label == n.label;
	}
}

public class Day12PassagePathing {
	static Graph<Node> graph;
	static Node start, end;
	public static void main(String[] args) {
		graph = new Graph<>();
		var buf = new BufferedReader(new InputStreamReader(System.in));
		var maplabel = new HashMap<String, Node>();
		try {
			while(true) {
				var s = buf.readLine();
				if (s == null || s.isEmpty()) break;
				//System.out.println("read line: " + s);
				var labels = s.split("-");
				//System.out.printf("split: %s - %s\n", labels[0], labels[1]);
				Node v1;
				if (!maplabel.containsKey(labels[0])) {
					v1 = new Node(labels[0]);
					maplabel.put(labels[0], v1);
				} else {
					v1 = maplabel.get(labels[0]);
				}
				Node v2;
				if (!maplabel.containsKey(labels[1])) {
					v2 = new Node(labels[1]);
					maplabel.put(labels[1], v2);
				} else {
					v2 = maplabel.get(labels[1]);
				}
				graph.addEdge(v1, v2);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		start = maplabel.get("start");
		end = maplabel.get("end");
		/*
		for (var node : graph.nodes.toArray(Node[]::new)) {
			System.out.println(node.label);
		}
		*/
		var paths = graph.paths(start, end);
		if (paths == null) {
			System.out.println("node start or end is not available");
		} else {
			for (var i = 0; i < paths.size(); i++) {
				for (var j = 0; j < paths.get(i).size(); j++) {
					var node = paths.get(i).get(j);
					System.out.printf("%s ", node);
				}
				System.out.println();
			}
			System.out.println("The total paths are " + paths.size());
			/*
			System.out.println("Edges:");
			for (var i = 0; i < graph.edges.size(); i++) {
				System.out.println(graph.edges.get(i));
			}
			*/
		}
	}
}
