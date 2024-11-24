import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.stream.IntStream;

class Range {
	int from, to;

	Range() {
		from = Integer.MIN_VALUE;
		to = Integer.MAX_VALUE;
	}

	Range(int from, int to) {
		this.from = from;
		this.to = to;
	}

	public String toString() {
		return String.format("%d..%d", from, to);
	}
}

class Step {
	Range x, y, z;
	boolean set;

	Step() {
		set = true;
		x = new Range();
		y = new Range();
		z = new Range();
	}

	Step(boolean set) {
		this.set = set;
	}

	Step(char c, int from, int to) {
		set = true;
		var range = new Range(from, to);
		if (c == 'x') {
			x = range;
		} else if (c == 'y') {
			y = range;
		} else {
			z = range;
		}
	}

	Step(Range x, Range y, Range z) {
		set = true;
		this.x = x;
		this.y = x;
		this.z = x;
	}

	public String toString() {
		return String.format("%s(x=%s, y=%s, z=%s)", set? "on" : "off", x, y, z);
	}
}

public class Day22ReactorReboot {
	public static void main(String[] args) {
		var cuboids = IntStream.rangeClosed(-50, 50).mapToObj((i) -> {
			return IntStream.rangeClosed(-50, 50).mapToObj(ii -> {
				return IntStream.rangeClosed(-50, 50).mapToObj(iii -> false).toArray(Boolean[]::new);
			}).toArray(Boolean[][]::new);
		}).toArray(Boolean[][][]::new);
		var buf = new BufferedReader(new InputStreamReader(System.in));
		try {
			while(true) {
				var s = buf.readLine();
				if (s == null || s.isEmpty()) break;
				var step = new Step();
				var subindex = 0;
				if (s.startsWith("on")) {
					step.set = true;
					subindex = 3;
				} else {
					step.set = false;
					subindex = 4;
				}
				var ss = s.substring(subindex).split(",");
				for (var sss : ss) {
					//System.out.println("sss: " + sss);
					assignRange(step, sss);
				}
				System.out.println("step: " + step);
				applyStep(cuboids, step);
			}
		} catch(Exception e) {
			e.printStackTrace();
		}
		var total = 0;
		for (var i = 0; i < cuboids.length; i++) {
			for (var j = 0; j < cuboids[i].length; j++) {
				for (var k = 0; k < cuboids[i][j].length; k++) {
					if (cuboids[i][j][k]) total++;
				}
			}
		}
		System.out.println("Total cuboids on after initialization steps are " + total);
	}

	static int toIndex(int pos) {
		return pos + 50;
	}

	static void assignRange(Step step, String sss) {
		var s = sss.split("=");
		var ss = s[1].split("\\.\\.");
		var range = new Range(Integer.parseInt(ss[0]), Integer.parseInt(ss[1]));
		//System.out.printf("%s=%s\n", s[0], range);
		s[0] = s[0].trim();
		if (s[0].equalsIgnoreCase("x")) {
			step.x = range;
		} else if (s[0].equalsIgnoreCase("y")) {
			step.y = range;
		} else {
			step.z = range;
		}
	}
	
	static boolean checkRanges(Step step) {
		int[] froms = {step.x.from, step.y.from, step.z.from};
		int[] tos = {step.x.to, step.y.to, step.z.to};
		for (var from : froms) {
			if (from < -50) return false;
		}
		for (var to : tos) {
			if (to > 50) return false;
		}
		return true;
	}

	static void applyStep(Boolean[][][] cuboids, Step step) {
		if (!checkRanges(step)) return;

		for (var i = step.x.from; i <= step.x.to; i++) {
			for (var j = step.y.from; j <= step.y.to; j++) {
				for (var k = step.z.from; k <= step.z.to; k++) {
					cuboids[toIndex(i)][toIndex(j)][toIndex(k)] = step.set;
				}
			}
		}
	}
}
