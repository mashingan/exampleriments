import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Day01_SonarSweep {
	public static void main(String []args) {
		var bufread = new BufferedReader(new InputStreamReader(System.in));
		int increment = 0, prev = Integer.MIN_VALUE;
		for (var count = 1;; count++) {
			try {
				var s = bufread.readLine();
				if (s == null || s.isEmpty()) { break; }
				var curr = Integer.parseInt(s);
				if (curr > prev && count > 1) {
					increment++;
				}
				prev = curr;
			} catch (NullPointerException npe) {
				break;
			} catch (Exception e) {
				e.printStackTrace(System.out);
				break;
			}
		}
		System.out.println("The increment is " + increment);
	}
}
