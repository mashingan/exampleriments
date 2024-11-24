import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;

public class Day03BinaryDiagnostic {
	static ArrayList<Boolean> gammaRate = new ArrayList<Boolean>();
	static ArrayList<Boolean> epsilonRate = new ArrayList<Boolean>();
	public static void main(String[] args) {
		var bufread = new BufferedReader(new InputStreamReader(System.in));
		var diagnostic = new ArrayList<String>();
		while (true) {
			try {
				var s = bufread.readLine();
				if (s == null || s.isEmpty()) { break; }
				diagnostic.add(s);
			} catch(Exception e) {
				e.printStackTrace();
				break;
			}
		}
		for (var i = 0; i < diagnostic.get(0).length(); i++) {
			getLeastMost(diagnostic, i);
		}
		var gamma = toInteger(gammaRate);
		var epsilon = toInteger(epsilonRate);
		System.out.println("gamma: " + gamma + ", epsilon: " + epsilon);
		System.out.println("power consumption: " + gamma * epsilon);
	}

	static void getLeastMost(ArrayList<String> diagnostic, int pos) {
		int count1 = 0, count0 = 0;
		for (var diag : diagnostic.toArray()) {
			var s = (String)diag;
			if (s.charAt(pos) == '0') {
				count0++;
			} else {
				count1++;
			}
		}

		Boolean gamma = false, epsilon = false;
		if (count1 > count0) {
			gamma = true;
		} else {
			epsilon = true;
		}
		gammaRate.add(gamma);
		epsilonRate.add(epsilon);
	}

	static int toInteger(ArrayList<Boolean> list) {
		int num = 0;
		for (var i = 0; i < list.size(); i++) {
			num = (num << 1) + (list.get(i) ? 1 : 0);
		}
		return num;
	}
}
