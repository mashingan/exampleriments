import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.stream.Stream;

public class Day06LanternFishV2 {
	final static int targetDay = 80;
	public static void main(String []args) {
		try {
			var lifecycle = Stream.generate(() -> 0).limit(9).mapToInt(Integer::intValue).toArray();
			var fishes = Arrays.stream((new BufferedReader(new InputStreamReader(System.in))).
					readLine().split(",")).
				mapToInt(Integer::parseInt).toArray();
			for (var i = 0; i < fishes.length; i++) {
				lifecycle[fishes[i]]++;
			}
			//System.out.println("day  0: current life cycle: " + Arrays.stream(lifecycle).boxed().toList().toString());
			stepsLifeCycle(targetDay, lifecycle);
			System.out.println("The total fishes after 80 steps are " + Arrays.stream(lifecycle).sum());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	static void stepsLifeCycle(int targetDay, int[] arr) {
		var respawnCycle = arr.length - 3;
		var lastCycle = arr.length - 1;
		for (var step = 0; step < targetDay; step++) {
			var spawner = arr[0];
			for (var i = 0; i < arr.length-1; i++) {
				var temp = arr[i+1];
				arr[i+1] = arr[i];
				arr[i] = temp;
			}
			arr[respawnCycle] += spawner;
			arr[lastCycle] = spawner;
			//System.out.printf("day %2d: current life cycle: %s\n", step+1,
					//Arrays.stream(arr).boxed().toList().toString());
		}
	}
}
