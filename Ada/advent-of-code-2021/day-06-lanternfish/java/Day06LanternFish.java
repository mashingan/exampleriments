import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.stream.Collectors;

public class Day06LanternFish {
	final static int targetDay = 80;
	public static void main(String []args) {
		try {
			var fishes = Arrays.stream((new BufferedReader(new InputStreamReader(System.in))).
					readLine().split(",")).
				mapToInt(Integer::parseInt).
				boxed().toList().
				stream().
				collect(Collectors.toCollection(ArrayList::new));
			for (var i = 0; i < targetDay; i++) {
				stepLifecycle(fishes);
			}
			System.out.println("The total fishes after 80 steps are " + fishes.size());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	static void stepLifecycle(ArrayList<Integer> list) {
		var size = list.size();
		for (var i = 0; i < size; i++) {
			var el = list.get(i);
			if (el == 0) {
				list.set(i, 6);
				list.add(8);
			} else {
				el--;
				list.set(i, el);
			}
		}
	}
}
