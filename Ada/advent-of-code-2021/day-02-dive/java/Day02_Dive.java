import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Day02_Dive {
	public static void main(String []args) {
		final var forward = "forward";
		final var down = "down";
		final var up = "up";
		int moveForward = 0, moveDown = 0;
		var bufread = new BufferedReader(new InputStreamReader(System.in));
		while (true) {
			try {
				var s = bufread.readLine();
				if (s == null || s.isEmpty()) { break; }
				if (s.startsWith(forward)) {
					moveForward += Integer.parseInt(s.substring(forward.length()+1));
				} else if (s.startsWith(up)) {
					moveDown -= Integer.parseInt(s.substring(up.length()+1));
				} else {
					moveDown += Integer.parseInt(s.substring(down.length()+1));
				}
			} catch(Exception e) {
				e.printStackTrace();
				break;
			}
		}
		System.out.println("Forward move: " + moveForward + " * down: " + moveDown +
				" = " + moveDown * moveForward);
	}
}
