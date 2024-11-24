import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.stream.IntStream;
import java.util.stream.Stream;

class Point {
	int x, y;
}

class Line {
	Point p1, p2;
}

class InvalidFormat extends Exception {
	Exception internalException;

	InvalidFormat() {
		internalException = new Exception("Invalid data provided");
	}

	InvalidFormat(String s) {
		internalException = new Exception(s);
	}
}

public class Day05HydrothermalVenture {
	static int[][] themap;
	public static void main(String []args) {
		themap = Stream.generate(() -> IntStream.generate(() -> 0).
				limit(10).toArray()).
			limit(10).toArray(int[][]::new);
		var buf = new BufferedReader(new InputStreamReader(System.in));
		try {
			while (true) {
				var s = buf.readLine();
				if (s == null || s.isEmpty()) { break; }
				var ss = s.split(" -> ");
				if (ss.length < 2) {
					throw new InvalidFormat("ss.length (" + ss.length + ") < 2");
				}
				var line = new Line();
				line.p1 = parsePoint(ss[0]);
				line.p2 = parsePoint(ss[1]);
				drawLine(line);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		for (var iv : themap) {
			for (var jv : iv) {
				System.out.printf("%2d", jv);
			}
			System.out.println();
		}
		var overlapped = calcOverlap();
		System.out.println("The overlap line points is " + overlapped);
	}

	static Point parsePoint(String s) {
		var res = new Point();
		var ss = s.split(",");
		try {
			res.x = Integer.parseInt(ss[0]);
			res.y = Integer.parseInt(ss[1]);
		} catch (Exception e) {}
		return res;
	}

	static void drawLine(Line line) {
		if (line.p1.x != line.p2.x && line.p1.y != line.p2.y) return;
		var xmax = Integer.max(line.p1.x, line.p2.x);
		var xmin = Integer.min(line.p1.x, line.p2.x);
		var ymax = Integer.max(line.p1.y, line.p2.y);
		var ymin = Integer.min(line.p1.y, line.p2.y);
		for (var i = ymin; i <= ymax; i++) {
			for (var j = xmin; j <= xmax; j++) {
				themap[i][j]++;
			}
		}
	}

	static int calcOverlap() {
		int[] initial = {0};
		var optres =  Arrays.stream(themap).reduce(initial, (acc, arr) -> {
			var nextval = Arrays.stream(arr).reduce(0, (acc2, val) ->
					(val > 1) ? acc2+1 : acc2);
			acc[0] += nextval;
			return acc;
		});
		return optres[0];
		/*
		int count = 0;
		for (var i = 0; i < 10; i++) {
			for (var j = 0; j < 10; j++) {
				if (themap[j][i] > 1) count++;
			}
		}
		return count;
		*/
	}
}
