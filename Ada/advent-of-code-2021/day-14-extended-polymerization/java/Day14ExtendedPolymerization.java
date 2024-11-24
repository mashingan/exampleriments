import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.stream.IntStream;

class Polymerization {
	int[] occurences;
	ArrayList<Character> sequence;
	HashMap<String, Character> formula;

	Polymerization() {
		occurences = IntStream.range(0, 26).map(i->0).toArray();
		sequence = new ArrayList<>();
		formula = new HashMap<>();
	}

	public String toString() {
		return sequence.toString();
	}
}

public class Day14ExtendedPolymerization {
	public static void main(String[] args) {
		var buf = new BufferedReader(new InputStreamReader(System.in));
		var polymer = new Polymerization();
		try {
			var s = buf.readLine();
			for (var c : s.toCharArray()) {
				polymer.occurences[c - 'A']++;
				polymer.sequence.add(c);
			}

			buf.readLine(); // ignore empty line

			while (true) {
				s = buf.readLine();
				if (s == null || s.isEmpty()) break;
				var ss = s.split(" -> ");
				polymer.formula.put(ss[0], ss[1].charAt(0));
			}
		} catch(Exception e) {
			e.printStackTrace();
		}
		//System.out.println("formula");
		//for (var key : polymer.formula.keySet()) {
			//System.out.printf("%s -> %c\n", key, polymer.formula.get(key));
		//}
		System.out.println("Template: " + polymer);
		for (var i = 1; i <= 10; i++) {
			step(polymer);
			//System.out.printf("step %2d: %s\n", i, polymer);
		}
		var mostCommon = Integer.MIN_VALUE;
		var leastCommon = Integer.MAX_VALUE;
		for (var i : polymer.occurences) {
			if (i > mostCommon) {
				mostCommon = i;
			}

			if (i < leastCommon && i != 0) {
				leastCommon = i;
			}
		}
		System.out.println("The difference between most and least occurences after 10 steps is "
				+ (mostCommon - leastCommon));
	}

	static void step(Polymerization polymer) {
		var newseq = new ArrayList<Character>();
		newseq.add(polymer.sequence.get(0));
		for (var i = 1; i < polymer.sequence.size(); i++) {
			var c_1 = polymer.sequence.get(i-1);
			var c = polymer.sequence.get(i);
			var key = String.format("%c%c", c_1, c);
			if (polymer.formula.containsKey(key)) {
				var cc = polymer.formula.get(key);
				newseq.add(cc);
				polymer.occurences[cc - 'A']++;
			}
			newseq.add(c);
		}
		polymer.sequence = newseq;
	}
}
