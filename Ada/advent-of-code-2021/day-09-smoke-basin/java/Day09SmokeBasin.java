import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.stream.IntStream;

public class Day09SmokeBasin {
	public static void main(String []args) {
		var bread = new BufferedReader(new InputStreamReader(System.in));
		try {
			ArrayList<int[]> listmap = new ArrayList<>();
			while(true) {
				var s = bread.readLine();
				if (s == null || s.isEmpty()) break;
				var sc = s.toCharArray();
				var nums = IntStream.range(0, sc.length).map((c) -> sc[c] - '0').toArray();
				listmap.add(nums);
			}
			var map = listmap.toArray(int[][]::new);
			checkmap(map);
			var lowpoints = new ArrayList<Integer>();
			for (var i = 0; i < map.length; i++) {
				for (var j = 0; j < map[i].length; j++) {
					var num = map[i][j];
					var iup = Integer.max(i-1, 0);
					var idown = Integer.min(i+1, map.length-1);
					var jleft = Integer.max(j-1, 0);
					var jright = Integer.min(j+1, map[i].length-1);
					var lowerElevation = true;
					if (lowerElevation && iup != i) {
						if (map[iup][j] < num) {
							lowerElevation = false;
						}
					}

					if (lowerElevation && idown != i) {
						if (map[idown][j] < num) {
							lowerElevation = false;
						}
					}

					if (lowerElevation && jleft != j) {
						if (map[i][jleft] < num) {
							lowerElevation = false;
						}
					}
					if (lowerElevation && jright != j) {
						if (map[i][jright] < num) {
							lowerElevation = false;
						}
					}

					if (lowerElevation) {
						lowpoints.add(num);
					}
				}
			}
			var totalrisk = lowpoints.stream().reduce(0, (acc, val) -> acc + val + 1);
			System.out.println("Low points: " + lowpoints.toString());
			System.out.println("Total risk from low points is " + totalrisk);
		} catch(Exception e) {
			e.printStackTrace();
		}
	}

	static void checkmap(int[][] map) {
			for (var i = 0; i < map.length; i++) {
				for (var j = 0; j < map[i].length; j++) {
					System.out.print(map[i][j]);
				}
				System.out.println();
			}
	}
}
