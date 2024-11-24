import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;

public class Day07TreacheryWhales {
	public static void main(String []args) {
		try {
			var arr = Arrays.stream(
					(new BufferedReader(new InputStreamReader(System.in))).readLine().split(",")
					).mapToInt(Integer::parseInt).toArray();
			Arrays.sort(arr);
			var min = Integer.MAX_VALUE;
			var sum = 0;
			var pos = Integer.MAX_VALUE;
			for (var i = 0; i < arr.length; i++) {
				sum = 0;
				for (var j = 0; j < arr.length; j++) {
					sum += Math.abs(arr[i] - arr[j]);
				}
				if (sum < min) {
					pos = i;
					min = sum;
				}
			}
			System.out.printf("The least energy aligned position is at %d with cost: %d",
					arr[pos], min);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
