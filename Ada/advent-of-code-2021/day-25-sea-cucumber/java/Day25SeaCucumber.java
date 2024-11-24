import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.stream.Collectors;

public class Day25SeaCucumber {
	public static void main(String[] args) {
		var map = new ArrayList<StringBuffer>();
		var buf = new BufferedReader(new InputStreamReader(System.in));
		try {
			while(true) {
				var s = buf.readLine();
				if (s == null || s.isEmpty()) break;
				map.add(new StringBuffer(s));
			}
		} catch(Exception e) {
			e.printStackTrace();
		}
		System.out.println("Initial map");
		checkMap(map);
		/*
		var newmap = moveLeft(map);
		System.out.printf("Move left? %s\n", isMapMoved(newmap, map)? "yes": "no");
		checkMap(newmap);
		map = newmap;
		newmap = moveDown(map);
		System.out.printf("Move down? %s\n", isMapMoved(newmap, map)? "yes": "no");
		checkMap(newmap);
		*/
		var moves = moveLeftDownUntilStop(map);
		System.out.println("Total moves until stop is: " + moves);

	}

	static ArrayList<StringBuffer> moveLeft(ArrayList<StringBuffer> map) {
		var newmap = new ArrayList<StringBuffer>();
		for (var i = 0; i < map.size(); i++) {
			var s = map.get(i);
			var subs = new StringBuffer(s);
			for (var j = 0; j < s.length()-1; j++) {
				var cc = s.charAt(j);
				var cn = s.charAt(j+1);
				if (cc == '>' && cn == '.') {
					subs.setCharAt(j, cn);
					subs.setCharAt(j+1, cc);
				}
			}
			var cc = s.charAt(s.length()-1);
			var cn = s.charAt(0);
			if (cc == '>' && cn == '.') {
				subs.setCharAt(s.length()-1, '.');
				subs.setCharAt(0, '>');
			}
			newmap.add(subs);
		}
		return newmap;
	}

	static ArrayList<StringBuffer> moveDown(ArrayList<StringBuffer> map) {
		var bufmap = map.stream()
			.map(s -> new StringBuffer(s))
			.collect(Collectors.toCollection(ArrayList::new));
		for (var i = 0; i < map.size()-1; i++) {
			var s = map.get(i);
			var ss = map.get(i+1);
			for (var j = 0; j < s.length(); j++) {
				var c = s.charAt(j);
				var cc = ss.charAt(j);
				if (c == 'v' && cc == '.') {
					bufmap.get(i).setCharAt(j, '.');
					bufmap.get(i+1).setCharAt(j, 'v');
				}
			}
		}
		var s = map.get(map.size()-1);
		var ss = map.get(0);
		for (var j = 0; j < s.length(); j++) {
			var c = s.charAt(j);
			var cc = ss.charAt(j);
			if (c == 'v' && cc == '.') {
				bufmap.get(map.size()-1).setCharAt(j, '.');
				bufmap.get(0).setCharAt(j, 'v');
			}
		}
		return bufmap;
	}

	static int moveLeftDownUntilStop(ArrayList<StringBuffer> map) {
		var newmap = map;
		var movedLeft = false;
		var movedDown = false;
		var stop = false;
		var movingCount = 0;
		while(!stop) {
			newmap = moveLeft(map);
			movedLeft = isMapMoved(newmap, map);
			map = newmap;
			newmap = moveDown(map);
			movedDown = isMapMoved(newmap, map);
			map = newmap;
			stop = !movedLeft && !movedDown;
			movingCount++;
		}
		System.out.println("Last configuration when map stops:");
		checkMap(map);
		return movingCount;
	}

	static void checkMap(ArrayList<StringBuffer> map) {
		for(var s : map.toArray(StringBuffer[]::new)) {
			System.out.println(s);
		}
	}

	static boolean isMapMoved(ArrayList<StringBuffer> newmap, ArrayList<StringBuffer> map) {
		if (map.size() != newmap.size()) return true;
		var moved = false;
		for (var i = 0; i < map.size(); i++) {
			if (!map.get(i).toString().equalsIgnoreCase(newmap.get(i).toString())) {
				moved = true;
				break;
			}
		}
		return moved;
	}
}
