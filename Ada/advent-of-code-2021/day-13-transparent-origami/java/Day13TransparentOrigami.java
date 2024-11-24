import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class Point {
	int x, y;

	Point(int x, int y) {
		this.x = x;
		this.y = y;
	}

	public String toString() {
		return String.format("%d,%d", x, y);
	}
}

class Map {
	int width, height, foldx, foldy;
	ArrayList<Point> dots;
	boolean foldyFirst;

	Map() {
		foldx = Integer.MAX_VALUE;
		foldy = Integer.MAX_VALUE;
		width = 0;
		height = 0;
		dots = new ArrayList<>();
	}
}

public class Day13TransparentOrigami {
	public static void main(String[] args) {
		var buf = new BufferedReader(new InputStreamReader(System.in));
		var map = new Map();
		try {
			while(true) {
				var s = buf.readLine();
				if (s != null && s.isEmpty()) break;
				var ss = s.split(",");
				System.out.println("ss: [" + Arrays.stream(ss).collect(Collectors.joining(", ")) + ']');
				var point = new Point(Integer.parseInt(ss[0]), Integer.parseInt(ss[1]));
				if (map.width < point.x) {
					map.width = point.x;
				}
				if (map.height < point.y) {
					map.height = point.y;
				}
				map.dots.add(point);
			}
			assignFold(buf, map);
		} catch(Exception e) {
			e.printStackTrace();
		}
		checkPaper(map);
		if (map.foldyFirst) {
			foldVertical(map);
		} else {
			foldHorizontal(map);
		}
		var dotsAfterFirstFold = mapRepr(map);
		var counts = 0;
		for(var s : dotsAfterFirstFold) {
			for(var c : s.toCharArray()) {
				if (c == '#') {
					counts++;
				}
			}
		}
		System.out.println("The count after first fold is " + counts);
		checkPaper(map);
	}

	static void assignFold(BufferedReader buf, Map map) throws Exception {
		var s = buf.readLine();
		var ss = s.split(" ");
		var sequal = ss[ss.length-1].split("=");
		var yfirst = false;
		if (sequal[0] == "x") {
			map.foldyFirst = false;
			map.foldx = Integer.parseInt(sequal[1]);
		} else {
			map.foldy = Integer.parseInt(sequal[1]);
			map.foldyFirst = true;
			yfirst = true;
		}

		ss = buf.readLine().split(" ");
		sequal = ss[ss.length-1].split("=");
		if (yfirst) {
			map.foldx = Integer.parseInt(sequal[1]);
		} else {
			map.foldy = Integer.parseInt(sequal[1]);
		}
	}

	static void foldHorizontal(Map map) {
		var foldLeft = map.foldx > map.width/2;
		for (var i = 0; i < map.dots.size(); i++) {
			var p = map.dots.get(i);
			if (foldLeft) {
				if (p.x >= map.foldx) {
					p.x = map.foldx - Math.abs(map.foldx - p.x);
				} else {
					p.x -= map.foldx;
				}
			} else {
				if (p.x < map.foldx) {
					p.x = Math.abs(map.foldx - p.x);
				} else {
					p.x -= map.foldx;
				}
			}
			map.dots.set(i, p);
		}
		map.width = Integer.max(map.foldx, map.width/2);
	}

	static void foldVertical(Map map) {
		var foldUp = map.foldy > map.height/2;
		for (var i = 0; i < map.dots.size(); i++) {
			var p = map.dots.get(i);
			if (foldUp) {
				if (p.y >= map.foldy) {
					p.y = map.foldy - Math.abs(map.foldy - p.y);
				}
			} else {
				if (p.y < map.foldy) {
					p.y = Math.abs(map.foldy - p.y);
				} else {
					p.y -= map.foldy;
				}
			}
			if(!map.dots.contains(p)) {
				map.dots.set(i, p);
			}
		}
		map.height = Integer.max(map.foldy, map.height/2);
	}

	static void checkPaper(Map map) {
		System.out.printf("Map with width %d and height %d\n", map.width, map.height);
		System.out.printf("fold x: %d, fold y: %d with fold %c first\n",
				map.foldx, map.foldy, map.foldyFirst ? 'y' : 'x');
		var paper = Stream
			.generate(() -> new StringBuilder(".".repeat(map.width+1)))
			.limit(map.height+1)
			.toArray(StringBuilder[]::new);
		for (var p : map.dots.toArray(Point[]::new)) {
			//System.out.println("check point: " + p);
			paper[p.y].setCharAt(p.x, '#');
		}
		for (var s : paper) {
			System.out.println(s);
		}
	}

	static String[] mapRepr(Map map) {
		var paper = Stream
			.generate(() -> new StringBuilder(".".repeat(map.width+1)))
			.limit(map.height+1)
			.toArray(StringBuilder[]::new);
		for (var p : map.dots.toArray(Point[]::new)) {
			//System.out.println("check point: " + p);
			paper[p.y].setCharAt(p.x, '#');
		}
		return Arrays.stream(paper).map(sb -> sb.toString()).toArray(String[]::new);
	}
}
